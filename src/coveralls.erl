%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc coveralls
%%% @end
%%% @author Markus Näsman <markus@botten.org>
%%% @copyright 2013 (c) Markus Näsman <markus@botten.org>
%%% @license Copyright (c) 2013, Markus Näsman
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the <organization> nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL MARKUS NÄSMAN BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Module declaration

-module(coveralls).

%%=============================================================================
%% Exports

-export([ convert_file/3
        , convert_and_send_file/3
        , send/1
        ]).

%%=============================================================================
%% Records

-record(s, { importer      = fun cover:import/1
           , module_lister = fun cover:imported_modules/0
           , mod_info      = passthrough
           , file_reader   = fun file:read_file/1
           , analyser      = fun cover:analyse/3
           }).

%%=============================================================================
%% API functions

%% @doc Import and convert cover file `Filename` to a json string
%%      representation suitable to post to coveralls.
%%
%%      Note that this function will crash if the modules mentioned in
%%      `Filename` are not availabe on the node.
%% @end
-spec convert_file(string(), string(), string()) -> string().
convert_file(Filename, ServiceJobId, ServiceName) ->
  convert_file(Filename, ServiceJobId, ServiceName, #s{}).

%% @doc Import and convert cover file `Filename` to a json string and send the
%%      json to coveralls
%% @end
-spec convert_and_send_file(string(), string(), string()) -> ok.
convert_and_send_file(Filename, ServiceJobId, ServiceName) ->
  send(convert_file(Filename, ServiceJobId, ServiceName)).

%% @doc Send json string to coveralls
-spec send(string()) -> ok.
send(_Json) ->
  throw(not_implemented).

%%=============================================================================
%% Internal functions

convert_file(Filename, ServiceJobId, ServiceName, S) ->
  ok               = import(S, Filename),
  ConvertedModules = convert_modules(S),
  Str              =
    "{~n"
    "\"service_job_id\": \"~s\",~n"
    "\"service_name\": \"~s\",~n"
    "\"source_files\": ~s"
    "}",
  lists:flatten(
    io_lib:format(Str, [ServiceJobId, ServiceName, ConvertedModules])).

%%-----------------------------------------------------------------------------
%% Callback mockery

import(#s{importer=F}, File) ->
  F(File).

imported_modules(#s{module_lister=F}) ->
  F().

analyze(#s{analyser=F}, Mod) ->
  F(Mod, calls, line).

compile_info(#s{mod_info=passthrough}, Mod) -> Mod:module_info(compile);
compile_info(#s{mod_info=MockMod}, Mod)     -> MockMod(Mod).

read_file(#s{file_reader=F}, SrcFile) ->
  F(SrcFile).

%%-----------------------------------------------------------------------------
%% Converting modules

convert_modules(S) ->
  F                = fun(Mod) -> convert_module(Mod, S) end,
  "[\n" ++ join(lists:map(F, imported_modules(S)), ",\n") ++ "\n]\n".

convert_module(Mod, S) ->
  {ok, CoveredLines} = analyze(S, Mod),
  SrcFile            = proplists:get_value(source, compile_info(S, Mod)),
  Name               = filename:basename(SrcFile),
  {ok, Src}          = read_file(S, SrcFile),
  LinesCount         = count_lines(Src),
  Cov                = create_cov(CoveredLines, LinesCount),
  Str                =
    "{~n"
    "\"name\": \"~s\",~n"
    "\"source\": \"~s\",~n"
    "\"coverage\": ~p~n"
    "}",
  lists:flatten(io_lib:format(Str, [Name, Src, Cov])).

create_cov(_CoveredLines, [])                                    ->
  [];
create_cov(CoveredLines, LinesCount) when is_integer(LinesCount) ->
  create_cov(CoveredLines, lists:seq(1, LinesCount));
create_cov([{{_,LineNo},Count}|CoveredLines], [LineNo|LineNos])  ->
  [Count | create_cov(CoveredLines, LineNos)];
create_cov(CoveredLines, [_|LineNos])                            ->
  [null | create_cov(CoveredLines, LineNos)].

%%-----------------------------------------------------------------------------
%% Generic helpers

join([H], _Sep)  -> H;
join([H|T], Sep) ->
  H++Sep++join(T, Sep).

count_lines(<<>>) ->
  0;
count_lines(B)    ->
  length(
    lists:filter(
      fun(X) -> X =/= <<>> end,
      binary:split(B, <<"\n">>, [global]))).

%%=============================================================================
%% Tests

-include_lib("eunit/include/eunit.hrl").

count_lines_test_() ->
  [ ?_assertEqual(0, count_lines(<<>>))
  , ?_assertEqual(1, count_lines(<<"foo">>))
  , ?_assertEqual(1, count_lines(<<"bar\n">>))
  , ?_assertEqual(2, count_lines(<<"foo\nbar">>))
  , ?_assertEqual(2, count_lines(<<"foo\nbar\n">>))
  ].

create_cov_test() ->
  ?assertEqual([null, 3, null, 4, null],
               create_cov([{{foo, 2}, 3}, {{foo, 4}, 4}], 5)).

join_test_() ->
  [ ?_assertEqual("a,b"   , join(["a","b"], ","))
  , ?_assertEqual("a,b,c" , join(["a","b","c"], ","))
  ].

convert_module_test() ->
  Expected =
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\n  4\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "}",
  ?assertEqual(Expected, lists:flatten(convert_module('example.rb', mock_s()))).

convert_modules_test() ->
  Expected =
    "[\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\n  4\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "},\n"
    "{\n"
    "\"name\": \"two.rb\",\n"
    "\"source\": \"def seven\n  eight\n  nine\nend\",\n"
    "\"coverage\": [null,1,0,null]\n"
    "}\n"
    "]\n",
  ?assertEqual(Expected,
               convert_modules(mock_s())).

convert_file_test() ->
  Expected =
    "{\n"
    "\"service_job_id\": \"1234567890\",\n"
    "\"service_name\": \"travis-ci\",\n"
    "\"source_files\": [\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\n  4\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "},\n"
    "{\n"
    "\"name\": \"two.rb\",\n"
    "\"source\": \"def seven\n  eight\n  nine\nend\",\n"
    "\"coverage\": [null,1,0,null]\n"
    "}\n"
    "]\n"
    "}",
  ?assertEqual(Expected,
               convert_file("example.rb", "1234567890", "travis-ci", mock_s())).

mock_s() ->
  #s{ importer      =
        fun(_) -> ok end
    , module_lister =
        fun() -> ['example.rb', 'two.rb'] end
    , mod_info      =
        fun('example.rb') -> [{source,"/foo/example.rb"}];
           ('two.rb')     -> [{source,"/foo/two.rb"}]
        end
    , file_reader   =
        fun("/foo/example.rb") ->
            {ok, <<"def four\n  4\nend">>};
           ("/foo/two.rb")     ->
            {ok, <<"def seven\n  eight\n  nine\nend">>}
        end
    , analyser      =
        fun('example.rb', calls, line) -> {ok, [{{'example.rb', 2}, 1}]};
           ('two.rb', calls, line)     -> {ok, [ {{'two.rb', 2}, 1}
                                               , {{'two.rb', 3}, 0}
                                               ]
                                          }
        end
    }.

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

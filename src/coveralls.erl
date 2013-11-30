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
           , mod_info      = fun module_info_compile/1
           , file_reader   = fun file:read_file/1
           , analyser      = fun cover:analyse/3
           , poster        = fun httpc:request/4
           , poster_init   = fun poster_init/0
           }).

%%=============================================================================
%% Defines

-define(COVERALLS_URL, "https://coveralls.io/api/v1/jobs").
%%-define(COVERALLS_URL, "http://127.0.0.1:8080").

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
  convert_and_send_file(Filename, ServiceJobId, ServiceName, #s{}).

%% @doc Send json string to coveralls
-spec send(string()) -> ok.
send(Json) -> send(Json, #s{}).

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

convert_and_send_file(Filename, ServiceJobId, ServiceName, S) ->
  send(convert_file(Filename, ServiceJobId, ServiceName, S), S).

send(Json, #s{poster=Poster, poster_init=Init}) ->
  ok       = Init(),
  Boundary = "----------" ++ integer_to_list(random:uniform(1000)),
  Type     = "multipart/form-data; boundary=" ++ Boundary,
  Body     = to_body(Json, Boundary),
  R        = Poster(post, {?COVERALLS_URL, [], Type, Body}, [], []),
  {ok, {{_, ReturnCode, _}, _, Message}} = R,
  case ReturnCode of
    200      -> ok;
    ErrCode  -> throw({error, {ErrCode, Message}})
  end.

%%-----------------------------------------------------------------------------
%% HTTP helpers

to_body(Json, Boundary) ->
  "--" ++ Boundary ++ "\r\n" ++
    "Content-Disposition: form-data; name=\"json_file\"; "
    "filename=\"json_file.json\" \r\n"
    "Content-Type: application/octet-stream\r\n\r\n"
    ++ Json ++ "\r\n" ++ "--" ++ Boundary ++ "--" ++ "\r\n".

%%-----------------------------------------------------------------------------
%% Callback mockery

import(#s{importer=F}, File) -> F(File).

imported_modules(#s{module_lister=F}) -> F().

analyze(#s{analyser=F}, Mod) -> F(Mod, calls, line).

compile_info(#s{mod_info=F}, Mod) -> F(Mod).

module_info_compile(Mod) -> Mod:module_info(compile).

read_file(#s{file_reader=F}, SrcFile) -> F(SrcFile).

poster_init() ->
  ok = inets_init(),
  ok = ssl_init().

ssl_init() ->
  case ssl:start() of
    {error,{already_started,_}} -> ok;
    ok                          -> ok
  end.

inets_init() ->
  case inets:start() of
    {error,{already_started,_}} -> ok;
    ok                          -> ok
  end.

%%-----------------------------------------------------------------------------
%% Converting modules

convert_modules(S) ->
  F = fun(Mod) -> convert_module(Mod, S) end,
  "[\n" ++ join(lists:map(F, imported_modules(S)), ",\n") ++ "\n]\n".

convert_module(Mod, S) ->
  {ok, CoveredLines0} = analyze(S, Mod),
  %% Remove strange 0 indexed line
  FilterF             = fun({{_, X}, _}) -> X =/= 0 end,
  CoveredLines        = lists:filter(FilterF, CoveredLines0),
  SrcFile             = proplists:get_value(source, compile_info(S, Mod)),
  {ok, SrcBin}        = read_file(S, SrcFile),
  Src0                = lists:flatten(io_lib:format("~s", [SrcBin])),
  LinesCount          = count_lines(Src0),
  Cov                 = create_cov(CoveredLines, LinesCount),
  Str                 =
    "{~n"
    "\"name\": \"~s\",~n"
    "\"source\": \"~s\",~n"
    "\"coverage\": ~p~n"
    "}",
  Src                = replace_newlines(
                         replace_quotes(
                           replace_escape(Src0, "\\\\"),
                           "\\\""),
                         "\\n"),
  lists:flatten(io_lib:format(Str, [SrcFile, Src, Cov])).

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

count_lines("")      -> 1;
count_lines("\n")    -> 1;
count_lines([$\n|S]) -> 1+count_lines(S);
count_lines([_|S])   -> count_lines(S).

join([H], _Sep)  -> H;
join([H|T], Sep) -> H++Sep++join(T, Sep).

replace_escape("", _)      -> "";
replace_escape([$\\|S], A) -> A ++ replace_escape(S,A);
replace_escape([E|S], A)   -> [E|replace_escape(S,A)].

replace_newlines("", _)        -> "";
replace_newlines("\n" ++ S, A) -> A ++ replace_newlines(S, A);
replace_newlines([E|S], A)     -> [E|replace_newlines(S,A)].

replace_quotes("", _)     -> "";
replace_quotes([$"|S], A) -> A ++ replace_quotes(S, A);
replace_quotes([E|S], A)  -> [E|replace_quotes(S,A)].

%%=============================================================================
%% Tests

-include_lib("eunit/include/eunit.hrl").

convert_file_test() ->
  Expected =
    "{\n"
    "\"service_job_id\": \"1234567890\",\n"
    "\"service_name\": \"travis-ci\",\n"
    "\"source_files\": [\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\\n  4\\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "},\n"
    "{\n"
    "\"name\": \"two.rb\",\n"
    "\"source\": \"def seven\\n  eight\\n  nine\\nend\",\n"
    "\"coverage\": [null,1,0,null]\n"
    "}\n"
    "]\n"
    "}",
  ?assertEqual(Expected, convert_file("example.rb",
                                      "1234567890",
                                      "travis-ci",
                                      mock_s())).

convert_and_send_file_test() ->
  Expected =
    "{\n"
    "\"service_job_id\": \"1234567890\",\n"
    "\"service_name\": \"travis-ci\",\n"
    "\"source_files\": [\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\\n  4\\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "},\n"
    "{\n"
    "\"name\": \"two.rb\",\n"
    "\"source\": \"def seven\\n  eight\\n  nine\\nend\",\n"
    "\"coverage\": [null,1,0,null]\n"
    "}\n"
    "]\n"
    "}",
  ?assertEqual(ok, convert_and_send_file("example.rb",
                                         "1234567890",
                                         "travis-ci",
                                         mock_s(Expected))).

send_test_() ->
  Expected =
    "{\n"
    "\"service_job_id\": \"1234567890\",\n"
    "\"service_name\": \"travis-ci\",\n"
    "\"source_files\": [\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\\n  4\\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "}\n]\n}",
  [ ?_assertEqual(ok, send(Expected, mock_s(Expected)))
  , ?_assertThrow({error, {_,_}}, send("foo", mock_s("bar")))
  ].

%%-----------------------------------------------------------------------------
%% Generic helpers tests

count_lines_test_() ->
  [ ?_assertEqual(1, count_lines(""))
  , ?_assertEqual(1, count_lines("foo"))
  , ?_assertEqual(1, count_lines("bar\n"))
  , ?_assertEqual(2, count_lines("foo\nbar"))
  , ?_assertEqual(3, count_lines("foo\n\nbar"))
  , ?_assertEqual(2, count_lines("foo\nbar\n"))
  ].

join_test_() ->
  [ ?_assertEqual("a,b"   , join(["a","b"], ","))
  , ?_assertEqual("a,b,c" , join(["a","b","c"], ","))
  ].

replace_newlines_test() ->
 ?assertEqual("foobarfoo", replace_newlines("foo\nfoo", "bar")).

replace_escape_test() ->
  ?assertEqual("foobarfoo", replace_escape("foo\\foo", "bar")).

replace_quotes_test() ->
  ?assertEqual("foobarfoo", replace_quotes("foo\"foo", "bar")).

%%-----------------------------------------------------------------------------
%% Converting modules tests

create_cov_test() ->
  ?assertEqual([null, 3, null, 4, null],
               create_cov([{{foo, 2}, 3}, {{foo, 4}, 4}], 5)).

convert_module_test() ->
  Expected =
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\\n  4\\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "}",
  ?assertEqual(Expected, lists:flatten(convert_module('example.rb', mock_s()))).

convert_modules_test() ->
  Expected =
    "[\n"
    "{\n"
    "\"name\": \"example.rb\",\n"
    "\"source\": \"def four\\n  4\\nend\",\n"
    "\"coverage\": [null,1,null]\n"
    "},\n"
    "{\n"
    "\"name\": \"two.rb\",\n"
    "\"source\": \"def seven\\n  eight\\n  nine\\nend\",\n"
    "\"coverage\": [null,1,0,null]\n"
    "}\n"
    "]\n",
  ?assertEqual(Expected,
               convert_modules(mock_s())).

%%-----------------------------------------------------------------------------
%% Setup helpers

mock_s() -> mock_s("").

mock_s(Json) ->
  #s{ importer      =
        fun(_) -> ok end
    , module_lister =
        fun() -> ['example.rb', 'two.rb'] end
    , mod_info      =
        fun('example.rb') -> [{source,"example.rb"}];
           ('two.rb')     -> [{source,"two.rb"}]
        end
    , file_reader   =
        fun("example.rb") ->
            {ok, <<"def four\n  4\nend">>};
           ("two.rb")     ->
            {ok, <<"def seven\n  eight\n  nine\nend">>}
        end
    , analyser      =
        fun('example.rb' , calls, line) -> {ok, [ {{'example.rb', 2}, 1} ]};
           ('two.rb'     , calls, line) -> {ok, [ {{'two.rb', 2}, 1}
                                                , {{'two.rb', 3}, 0}
                                                ]
                                           }
        end
    , poster_init   =
        fun() -> ok end
    , poster        =
        fun(post, {_, _, _, Body}, _, _) ->
            case string:str(Body, Json) =/= 0 of
              true  -> {ok, {{"", 200, ""}, "", ""}};
              false -> {ok, {{"", 666, ""}, "", "Not expected"}}
            end
        end
    }.

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

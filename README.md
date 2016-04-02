coveralls-erl
=============
[![Build Status](https://travis-ci.org/markusn/coveralls-erl.png?branch=master)](https://travis-ci.org/markusn/coveralls-erl)
[![Coverage Status](https://coveralls.io/repos/markusn/coveralls-erl/badge.png?branch=master)](https://coveralls.io/r/markusn/coveralls-erl?branch=master)

Erlang module to convert and send cover data to coveralls.

## Example usage: rebar and Travis CI                                                                           
In order to use coveralls-erl + Travis CI in your project you will need to add the following lines to your `rebar.config.script`:                                                                                                
```erlang
case os:getenv("TRAVIS") of
  "true" ->
    JobId   = os:getenv("TRAVIS_JOB_ID"),
    CONFIG1 = lists:keystore(coveralls_service_job_id, 1, CONFIG, {coveralls_service_job_id, JobId}),
    lists:keystore(plugins, 1, CONFIG1, {plugins, [rebar_coveralls]});
  _ ->
    CONFIG
end.
```

This will ensure that rebar_coveralls will have access to the needed JobId and that the plugin is only run from Travis CI.

You will also need to add the following lines to your `rebar.config`:
```erlang                                                                                                       
{deps                   , [ { coveralls
                            , ".*"
                            , {git, "git://github.com/markusn/coveralls-erl.git", "master"}
                            }
                          ]}.
{plugin_dir             , "deps/coveralls/src"}.
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{coveralls_coverdata    , ".eunit/eunit.coverdata"}. % or a list of files
{coveralls_service_name , "travis-ci"}.
```
These changes will add `coveralls-erl` as a dependency, tell `rebar` where to find the plugin, make sure that the coverage data is produced and exported and configure `coveralls-erl` to use this data and the service `travis-ci`. 

With these modifications to your rebar configuration `rebar_coveralls` will run after EUnit or CT is finished and export the coverage data to coveralls.
If you don't want to export data to coveralls after EUnit or CT is finished you can disable it for each task:
```erlang
{do_coveralls_after_ct, false}.
{do_coveralls_after_eunit, false}.
```
and then use the `send-coveralls` task: `rebar skip_deps=true eunit ct send-coveralls`


NOTE: It may be a good idea to add your own `rebar` binary to your repository to ensure that Travis CI runs with a rebar binary that supports the needed `cover_export_enabled` option.

## Example usage: rebar3 and Travis CI      
Example `rebar.config.script`:                                                                                      

```erlang
case os:getenv("TRAVIS") of
  "true" ->
    JobId   = os:getenv("TRAVIS_JOB_ID"),
    lists:keystore(coveralls_service_job_id, 1, CONFIG, {coveralls_service_job_id, JobId});
  _ ->
    CONFIG
end.
```

Example `rebar.config`:

```erlang                                                                                                       
{plugins                , [{coveralls, {git, "https://github.com/markusn/coveralls-erl", "master"}}]}.
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{coveralls_coverdata    , "_build/test/cover/eunit.coverdata"}. % or a list of files
{coveralls_service_name , "travis-ci"}.
```

And you send the coverdata to coveralls by issuing: `rebar3 coveralls send`

## Author
Markus Ekholm (markus at botten dot org).

## License
3-clause BSD. For details see `COPYING`.


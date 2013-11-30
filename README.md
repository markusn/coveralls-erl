coveralls-erl
=============
[![Build Status](https://travis-ci.org/markusn/coveralls-erl.png?branch=master)](https://travis-ci.org/markusn/coveralls-erl)
[![Coverage Status](https://coveralls.io/repos/markusn/coveralls-erl/badge.png?branch=master)](https://coveralls.io/r/markusn/coveralls-erl?branch=master)

Erlang module to convert and send cover data to coveralls.

## Example usage: rebar and Travis CI                                                                           
In order to use coveralls-erl + Travis CI in your project you will need to add the following lines to your reba\
r.config.script:                                                                                                
```erlang                                                                                                       
case os:getenv("TRAVIS_JOB_ID") of                                                                              
  false -> CONFIG;                                                                                              
  ""    -> CONFIG;                                                                                              
  JobId -> lists:keystore(coveralls_service_job_id, 1, CONFIG, {coveralls_service_job_id, JobId})               
end.                                                                                                            
```                                                                                                             
                                                                                                                
This will ensure that rebar_coveralls will have access to the needed JobId.

You will also need to add the following lines to your rebar.config:
```erlang                                                                                                       
{deps                   , [{coveralls, ".*", {git, "git://github.com/markusn/coveralls-erl.git", "master"}}]}.
{plugin_dir             , "deps/coveralls/src"}.
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{plugins                , [rebar_coveralls]}.
{coveralls_coverdata    , ".eunit/eunit.coverdata"}.
{coveralls_service_name , "travis-ci"}.
```                                                                                                             
With these modifications to your rebar configuration rebar_coveralls will run after eunit is finished and export the coverage data to coveralls.

## Author
Markus Näsman (markus at botten dot org).

## License
3-clause BSD. For details see `COPYING`.


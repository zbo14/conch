-module(host_SUITE).
-compile(export_all).
-include("include/codes.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Callbacks 

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [].

%% Testcase
%% TODO

%% Assertions

check(Expected) ->
    receive 
        Result ->
            ?assertEqual(Expected,Result)
    end.

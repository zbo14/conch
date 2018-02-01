-module(example).
-include("include/codes.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([client1/0,client2/0,client3/0]).
-define(GEORGE,<<"george">>).
-define(KRAMER,<<"kramer">>).
-define(NEWMAN,<<"newman">>).
-define(TOPIC,<<"nothing">>).
-define(CHAT_MSG,<<"I'm a marine biologist">>).
-define(ROOM_MSG,<<"No you're not!">>).
-define(MEMBER_MSG,<<"Hey, where's Jerry?">>).

%% Clients

client1() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?GEORGE),
    check_join_chat(?GEORGE),
    check_join_chat(?KRAMER),
    check_join_chat(?NEWMAN),
    ok = client:join_room(Pid,?TOPIC),
    check_join_room(?GEORGE),
    ok = client:send_chat(Pid,?CHAT_MSG),
    check_chat_msg(),
    check_join_room(?KRAMER),
    check_room_msg(),
    check_leave_room(?KRAMER),
    ok = client:leave_room(Pid,?TOPIC),
    ok = client:leave_chat(Pid).

client2() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?KRAMER),
    check_join_chat(?KRAMER),
    check_join_chat(?NEWMAN),
    check_chat_msg(),
    ok = client:join_room(Pid,?TOPIC),
    check_join_room(?KRAMER),
    ok = client:send_room(Pid,?TOPIC,?ROOM_MSG),
    check_room_msg(),
    ok = client:leave_room(Pid,?TOPIC),
    check_leave_chat(?GEORGE),
    check_member_msg(),
    ok = client:leave_chat(Pid).

client3() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?NEWMAN),
    check_join_chat(?NEWMAN),
    check_chat_msg(),
    check_leave_chat(?GEORGE),
    ok = client:send_member(Pid,?KRAMER,?MEMBER_MSG),
    check_leave_chat(?KRAMER),
    ok = client:leave_chat(Pid).

%% Assertions 

check(Expected) ->
    receive
        Result ->
            ?assertEqual(Expected,Result),
            client_util:log(Result)
    end.

check_join_chat(Alias) ->
    check({?JOIN_CHAT,Alias}).

check_join_room(Alias) ->
    check({Alias,?JOIN_ROOM,?TOPIC}).

check_leave_room(Alias) ->
    check({Alias,?LEAVE_ROOM,?TOPIC}).

check_leave_chat(Alias) ->
    check({Alias,?LEAVE_CHAT}).

check_chat_msg() ->
    check({?GEORGE,?SEND_CHAT,?CHAT_MSG}).

check_room_msg() ->
    check({?KRAMER,?SEND_ROOM,?TOPIC,?ROOM_MSG}).

check_member_msg() ->
    check({?NEWMAN,?SEND_MEMBER,?KRAMER,?MEMBER_MSG}).

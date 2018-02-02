-module(example).
-include("include/codes.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([george/0,kramer/0,newman/0]).
-define(GEORGE,<<"george">>).
-define(KRAMER,<<"kramer">>).
-define(NEWMAN,<<"newman">>).
-define(TOPIC,<<"nothing">>).
-define(CHAT_MSG,<<"I'm a marine biologist">>).
-define(ROOM_MSG,<<"No you're not!">>).
-define(MEMBER_MSG,<<"Hey, where's Jerry?">>).

%% Clients

george() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?GEORGE),
    check_join_chat(?GEORGE),
    check_join_chat(?KRAMER),
    check_join_chat(?NEWMAN),
    ok = client:members(Pid),
    check_members(),
    ok = client:join_room(Pid,?TOPIC),
    check_join_room(?GEORGE),
    ok = client:send_chat(Pid,?CHAT_MSG),
    check_chat_msg(),
    check_join_room(?KRAMER),
    check_room_msg(),
    check_leave_room(?KRAMER),
    ok = client:leave_room(Pid,?TOPIC),
    ok = client:leave_chat(Pid).

kramer() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?KRAMER),
    check_join_chat(?KRAMER),
    check_join_chat(?NEWMAN),
    check_chat_msg(),
    ok = client:join_room(Pid,?TOPIC),
    check_join_room(?KRAMER),
    ok = client:my_rooms(Pid),
    check_my_rooms_before(),
    ok = client:send_room(Pid,?TOPIC,?ROOM_MSG),
    check_room_msg(),
    ok = client:leave_room(Pid,?TOPIC),
    check_leave_chat(?GEORGE),
    check_member_msg(),
    ok = client:my_rooms(Pid),
    check_my_rooms_after(),
    ok = client:leave_chat(Pid).

newman() ->
    {ok,Pid} = client:run(),
    ok = client:join_chat(Pid,?NEWMAN),
    check_join_chat(?NEWMAN),
    check_chat_msg(),
    check_leave_chat(?GEORGE),
    ok = client:rooms(Pid),
    check_rooms(),
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
    check({?JOIN_ROOM,Alias,?TOPIC}).

check_members() ->
    check({?MEMBERS,[?GEORGE,?KRAMER,?NEWMAN]}).

check_my_rooms_before() ->
    check({?MY_ROOMS,[?TOPIC]}).

check_my_rooms_after() ->
    check({?MY_ROOMS,[]}).

check_rooms() ->
    check({?ROOMS,[?TOPIC]}).

check_leave_room(Alias) ->
    check({?LEAVE_ROOM,Alias,?TOPIC}).

check_leave_chat(Alias) ->
    check({?LEAVE_CHAT,Alias}).

check_chat_msg() ->
    check({?SEND_CHAT,?GEORGE,?CHAT_MSG}).

check_room_msg() ->
    check({?SEND_ROOM,?KRAMER,?TOPIC,?ROOM_MSG}).

check_member_msg() ->
    check({?SEND_MEMBER,?NEWMAN,?KRAMER,?MEMBER_MSG}).

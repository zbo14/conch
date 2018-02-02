-module(client_SUITE).
-compile(export_all).
-include("include/codes.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(ALIAS,<<"george_costanza">>).
-define(TOPIC,<<"nothing">>).
-define(TO,<<"kramer">>).
-define(PAYLOAD,<<"I'm a marine biologist">>).

%% Callbacks 

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [client_test].

%% Testcase

client_test(_Config) ->
    
    %% Run sshd
    ok = ssh:start(),
    Dir = code:priv_dir(host),
    {ok,_} = ssh:daemon(8889,[{system_dir,Dir},{user_dir,Dir},{subsystems,[{"subsystem",{subsystem,[self()]}}]}]),

    %% Start client
    {ok,Pid} = client:start(),

    join_chat(Pid),
    join_room(Pid),
    send_chat(Pid),
    send_room(Pid),
    send_member(Pid),
    leave_room(Pid),
    leave_chat(Pid),
    members(Pid),
    rooms(Pid),
    my_rooms(Pid).

%% Assertions

check(Expected) ->
    receive 
        Msg ->
            ?assertEqual(Expected,Msg)
    end.

join_chat(Pid) ->
    ok = client:join_chat(Pid,?ALIAS),
    Size = byte_size(?ALIAS),
    check(<<?JOIN_CHAT,Size,?ALIAS:Size/binary>>).

join_room(Pid) ->
    ok = client:join_room(Pid,?TOPIC),
    Size = byte_size(?TOPIC),
    check(<<?JOIN_ROOM,Size,?TOPIC:Size/binary>>).

send_chat(Pid) ->
    ok = client:send_chat(Pid,?PAYLOAD),
    Size = byte_size(?PAYLOAD),
    check(<<?SEND_CHAT,Size:32,?PAYLOAD:Size/binary>>).

send_room(Pid) ->
    ok = client:send_room(Pid,?TOPIC,?PAYLOAD),
    TSize = byte_size(?TOPIC),
    PSize = byte_size(?PAYLOAD),
    check(<<?SEND_ROOM,TSize,?TOPIC:TSize/binary,PSize:32,?PAYLOAD:PSize/binary>>).

send_member(Pid) ->
    ok = client:send_member(Pid,?TO,?PAYLOAD),
    TSize = byte_size(?TO),
    PSize = byte_size(?PAYLOAD),
    check(<<?SEND_MEMBER,TSize,?TO:TSize/binary,PSize:32,?PAYLOAD:PSize/binary>>).

leave_room(Pid) ->
    ok = client:leave_room(Pid,?TOPIC),
    Size = byte_size(?TOPIC),
    check(<<?LEAVE_ROOM,Size,?TOPIC:Size/binary>>).

leave_chat(Pid) ->
    ok = client:leave_chat(Pid),
    check(<<?LEAVE_CHAT>>).

members(Pid) ->
    ok = client:members(Pid),
    check(<<?MEMBERS>>).

rooms(Pid) ->
    ok = client:rooms(Pid),
    check(<<?ROOMS>>).

my_rooms(Pid) ->
    ok = client:my_rooms(Pid),
    check(<<?MY_ROOMS>>).
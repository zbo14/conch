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

    %% Join chat
    ok = client:join_chat(Pid,?ALIAS),
    joined_chat(),

    %% Join room
    ok = client:join_room(Pid,?TOPIC),
    joined_room(),

    %% Send chat
    ok = client:send_chat(Pid,?PAYLOAD),
    sent_chat(),

    %% Send room
    ok = client:send_room(Pid,?TOPIC,?PAYLOAD),
    sent_room(),

    %% Send member
    ok = client:send_member(Pid,?TO,?PAYLOAD),
    sent_member(),

    %% Leave room
    ok = client:leave_room(Pid,?TOPIC),
    left_room(),
    
    %% Leave chat
    ok = client:leave_chat(Pid),
    left_chat().

%% Assertions

joined_chat() ->
    Size = byte_size(?ALIAS),
    receive
        Msg ->
            ?assertEqual(Msg,<<?JOIN_CHAT,Size,?ALIAS:Size/binary>>)
    end.

joined_room() ->
    Size = byte_size(?TOPIC),
    receive
        Msg ->
            ?assertEqual(Msg,<<?JOIN_ROOM,Size,?TOPIC:Size/binary>>)
    end.

sent_chat() ->
    Size = byte_size(?PAYLOAD),
    receive 
        Msg ->
            ?assertEqual(Msg,<<?SEND_CHAT,Size:32,?PAYLOAD:Size/binary>>)
    end.

sent_room() ->
    TSize = byte_size(?TOPIC),
    PSize = byte_size(?PAYLOAD),
    receive 
        Msg ->
            ?assertEqual(Msg,<<?SEND_ROOM,TSize,?TOPIC:TSize/binary,PSize:32,?PAYLOAD:PSize/binary>>)
    end.

sent_member() ->
    TSize = byte_size(?TO),
    PSize = byte_size(?PAYLOAD),
    receive 
        Msg ->
            ?assertEqual(Msg,<<?SEND_MEMBER,TSize,?TO:TSize/binary,PSize:32,?PAYLOAD:PSize/binary>>)
    end.

left_room() ->
    Size = byte_size(?TOPIC),
    receive 
        Msg ->
            ?assertEqual(Msg,<<?LEAVE_ROOM,Size,?TOPIC:Size/binary>>)
    end.

left_chat() ->
    receive
        Msg ->
            ?assertEqual(Msg,<<?LEAVE_CHAT>>)
    end.
-module(client).
-include("include/codes.hrl").
-behaviour(ssh_channel).
-record(state,{alias=undefined,ch,cn,dir,haddr,hport,pid}).
-export([run/0,start/0,start_link/0,setup/0]).
-export([join_chat/2,leave_chat/1,join_room/2,leave_room/2,send_chat/2,send_room/3,send_member/3]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,handle_msg/2,handle_ssh_msg/2,terminate/2,code_change/3]).

send_msg(CH,CN,Msg) ->
    success = ssh_connection:subsystem(CN,CH,"subsystem",infinity),
    ssh_connection:send(CN,CH,Msg).

%% Client API

run() ->
    ok = ssh:start(),
    client:start_link().

setup() ->
    Dir = code:priv_dir(client),
    HAddr = application:get_env(client,haddr,"localhost"),
    HPort = application:get_env(client,hport,8889),
    {ok,CN} = ssh:connect(HAddr,HPort,[{user_dir,Dir}]),
    {ok,CH} = ssh_connection:session_channel(CN,infinity),
    State = #state{
        ch=CH,
        cn=CN,
        dir=Dir,
        haddr=HAddr,
        hport=HPort,
        pid=self()
    },
    {CN,CH,State}.

start() ->
    {CN,CH,State} = setup(),
    ssh_channel:start(CN,CH,?MODULE,[State]).

start_link() ->
    {CN,CH,State} = setup(),
    ssh_channel:start_link(CN,CH,?MODULE,[State]).

%% Requests

join_chat(Pid,<<Alias/binary>>) ->
    ssh_channel:call(Pid,{?JOIN_CHAT,Alias}).

leave_chat(Pid) ->
    ssh_channel:call(Pid,?LEAVE_CHAT).

join_room(Pid,<<Topic/binary>>) ->
    ssh_channel:call(Pid,{?JOIN_ROOM,Topic}).

leave_room(Pid,<<Topic/binary>>) ->
    ssh_channel:call(Pid,{?LEAVE_ROOM,Topic}).

send_chat(Pid,<<Payload/binary>>) ->
    ssh_channel:cast(Pid,{?SEND_CHAT,Payload}).

send_room(Pid,<<Topic/binary>>,<<Payload/binary>>) ->
    ssh_channel:cast(Pid,{?SEND_ROOM,Topic,Payload}).

send_member(Pid,<<To/binary>>,<<Payload/binary>>) ->
    ssh_channel:cast(Pid,{?SEND_MEMBER,To,Payload}).

%% Queries 
%% TODO

%% Callbacks 

init([State]) ->
    process_flag(trap_exit,true),
    {ok,State}.

handle_call({?JOIN_CHAT,Alias}=Request,_From,#state{alias=undefined,ch=CH,cn=CN}=State) ->
    Msg = client_util:encode(Request),
    ok = send_msg(CH,CN,Msg),
    NewState = State#state{alias=Alias},
    {reply,ok,NewState};

handle_call(Input,_From,#state{ch=CH,cn=CN}=State) ->
    Msg = client_util:encode(Input),
    ok = send_msg(CH,CN,Msg),
    {reply,ok,State}.

handle_cast(Input,#state{ch=CH,cn=CN}=State) -> 
    Msg = client_util:encode(Input),
    ok = send_msg(CH,CN,Msg),
    {noreply,State}.

handle_msg({ssh_channel_up,_CH,_CN},Sink) ->
    {ok,Sink}.

handle_ssh_msg({ssh_cm,_CN,{data,_CH,0,Msg}},#state{pid=Pid}=State) ->
    Result = client_util:decode(Msg),
    Pid ! Result,
    {ok,State};

handle_ssh_msg({ssh_cm,_,{signal,_,_}},State) ->
    {ok,State};

handle_ssh_msg({ssh_cm,_,{exit_status,CH,_Status}},State) ->
    {stop,CH,State}.

handle_info(Info,State) ->
    {stop,{unimplemented,info,Info},State}.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

terminate(Reason,State) ->
    io:fwrite("~p,~p~n",[Reason,State]),
    ok.
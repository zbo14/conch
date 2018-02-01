-module(subsystem).
-behaviour(ssh_daemon_channel).
-export([init/1,handle_msg/2,handle_ssh_msg/2,terminate/2]).

init([Pid]) ->
    {ok,Pid}.

handle_msg({ssh_channel_up,_CH,_CN},Pid) ->
    {ok,Pid}.

handle_ssh_msg({ssh_cm,_CN,{data,_CH,0,Msg}},Pid) ->
    Pid ! Msg,
    {ok,Pid};

handle_ssh_msg({ssh_cm,_CN,{data,_CH,1,Msg}},Pid) ->
    error_logger:format(standard_error,"~p~n",[binary_to_list(Msg)]),
    {ok,Pid};

handle_ssh_msg({ssh_cm,_CN,{eof,_CH}},Pid) ->
    {ok,Pid};

handle_ssh_msg({ssh_cm,_,{signal,_,_}},Pid) ->
    {ok,Pid};

handle_ssh_msg({ssh_cm,_,{exit_status,CH,_Status}},Pid) ->
    {stop,CH,Pid}.

terminate(_Reason,_Pid) ->
    ok.
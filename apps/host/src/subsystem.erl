-module(subsystem).
-behaviour(ssh_daemon_channel).
-export([init/1,handle_msg/2,handle_ssh_msg/2,terminate/2]).

init([Host]) ->
    {ok,Host}.

handle_msg({ssh_channel_up,_CH,_CN},Host) ->
    {ok,Host}.

handle_ssh_msg({ssh_cm,CN,{data,CH,0,Msg}},Host) ->
    ok = gen_server:cast(Host,{CH,CN,Msg}),
    {ok,Host};

handle_ssh_msg({ssh_cm,_CN,{data,_CH,1,Msg}},Host) ->
    error_logger:format(standard_error,"~p~n",[binary_to_list(Msg)]),
    {ok,Host};

handle_ssh_msg({ssh_cm,_CN,{eof,_CH}},Host) ->
    {ok,Host};

handle_ssh_msg({ssh_cm,_,{signal,_,_}},Host) ->
    {ok,Host};

handle_ssh_msg({ssh_cm,_,{exit_status,CH,_Status}},Host) ->
    {stop,CH,Host}.

terminate(_Reason,_Host) ->
    ok.

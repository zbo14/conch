%%%-------------------------------------------------------------------
%% @doc host top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(host_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
    RestartStrategy = {one_for_one, 5, 10},
    ChildSpec = {host, {host, start_link, []}, permanent, 5000, worker, [host]},
    {ok, {RestartStrategy, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

%%%-------------------------------------------------------------------
%% @doc Tic-Tac-Toe game  top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(tictactoe_sup).

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
init([]) ->
    Child = {tictactoe_server, {tictactoe_server, start_link, []}, permanent,
             brutal_kill, worker, [tictactoe_server]},
    {ok, { {one_for_one, 100, 1000}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================

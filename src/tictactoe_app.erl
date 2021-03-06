%%===================================================================
%% Copyright (c) 2017, Ramon Lastres
%% @doc Module implementing the application behaviour.
%% @end
%%===================================================================
-module(tictactoe_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    tictactoe_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

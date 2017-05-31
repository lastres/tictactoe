%%===================================================================
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

    Dispatch = cowboy_router:compile([
                {'_', [
                        {"/websocket", tictactoe_ws_handler, []}
                        ]}
                ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    tictactoe_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

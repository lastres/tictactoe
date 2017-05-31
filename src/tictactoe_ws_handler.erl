%%===================================================================
%% @doc This module implements the WS handler for Cowboy in order to
%% interact with the game server.
%% @end
%%===================================================================
-module(tictactoe_ws_handler).

-behaviour(cowboy_websocket_handler).

%% websocket callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%===================================================================
%% Websocket behaviour callbacks
%%===================================================================
init({tcp, http}, _Req, _Opts) ->
        {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        {ok, Req, undefined_state}.

websocket_handle({text, <<"reset">>}, Req, State) ->
    tictactoe_server:reset_game(),
    {reply, {text, <<"Game reset performed">>}, Req, State};
websocket_handle({text, <<"board">>}, Req, State) ->
    {ok, Board} = tictactoe_server:get_board_state(),
    {reply, {text, board_to_iolist(Board)}, Req, State};
websocket_handle({text, <<"status">>}, Req, State) ->
    {ok, Status} = tictactoe_server:game_status(),
    {reply, {text, atom_to_list(Status)}, Req, State};
websocket_handle({text, Input}, Req, State) ->
    Result = case try_match_board(Input) of
        {error, invalid_input} ->
            <<"Invalid input!">>;
        Board ->
            case tictactoe_server:make_move(Board) of
                {ok, NewBoard} ->
                    [<<"Move performed! ">>, <<"new board ->">>,
                     board_to_iolist(NewBoard)];
                {error, invalid_move} ->
                    <<"Invalid move!">>
            end
    end,
    {reply, {text, Result}, Req, State};
websocket_handle(_Data, Req, State) ->
        {ok, Req, State}.

websocket_info(_Info, Req, State) ->
        {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
        ok.

%%===================================================================
%% Internal functions
%%===================================================================

board_to_iolist(Board) ->
    lists:map(fun atom_to_list/1, Board).

try_match_board(Input) ->
    try
    Board = binary_to_list(Input),
    [_,_,_,_,_,_,_,_,_] = Board,
    lists:map(fun erlang:list_to_atom/1,
              lists:map(fun(X) -> [X] end, Board))
    catch
        _E:_R ->
            {error, invalid_input}
    end.

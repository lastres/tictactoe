%%===================================================================
%% @doc This module implements simple tests for the tictactoe game
%% server.
%% @end
%%===================================================================
-module(tictactoe_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("tictactoe.hrl").

list_of_tests() ->
    [fun complete_perfect_game/1,
     fun game_reset_test/1].

tictactoe_server_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     list_of_tests()}.

start() ->
    {ok, _} = application:ensure_all_started(tictactoe),
    ok = tictactoe_server:reset_game().
 
stop(_) ->
        ok.

%% Same as tictactoe test, a perfect game from both sides ends
%% on a draw situation.
complete_perfect_game(_) ->
    {_, B1} = make_perfect_move(?STARTBOARD),
    {ok, NB1} = tictactoe_server:make_move(B1),
    {_, B2} = make_perfect_move(NB1),
    {ok, NB2} = tictactoe_server:make_move(B2),
    {_, B3} = make_perfect_move(NB2),
    {ok, NB3} = tictactoe_server:make_move(B3),
    {_, B4} = make_perfect_move(NB3),
    {ok, NB4} = tictactoe_server:make_move(B4),
    {_, B5} = make_perfect_move(NB4),
    {ok, NB5} = tictactoe_server:make_move(B5),
    [?_assert(tictactoe:is_terminal_state(NB5)),
     ?_assertEqual(draw, tictactoe:who_wins(NB5)),
     ?_assertEqual({ok, draw}, tictactoe_server:game_status()),
     ?_assertEqual({error, game_finished}, tictactoe_server:make_move(NB5)),
     ?_assertEqual({ok, NB5}, tictactoe_server:get_board_state())].

game_reset_test(_) ->
     {_, B} = make_perfect_move(?STARTBOARD),
     {ok, _NB} = tictactoe_server:make_move(B),
     ok = tictactoe_server:reset_game(),
     [?_assertEqual({ok, ?STARTBOARD}, tictactoe_server:get_board_state())].

%% Uses the minimax algorithm to make a perfect move.
make_perfect_move(Board) ->
    tictactoe:take_a_master_move(Board, 'X').

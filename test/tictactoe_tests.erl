%%===================================================================
%% @doc This module implements very simple Eunit tests for the
%% minimax TicTacToe AI algorithm.
%% @end
%%===================================================================
-module(tictactoe_tests).

-define(STARTBOARD, ['E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E']).

-include_lib("eunit/include/eunit.hrl").

%% if the algorithm plays against itself, the game goes up to the
%% maximum number of movements and it must end on a draw situation.
complete_game_test_() ->
    {_Value, Board1} = tictactoe:take_a_master_move(?STARTBOARD, 'X'),
    {_Value, Board2} = tictactoe:take_a_master_move(Board1, 'O'),
    {_Value, Board3} = tictactoe:take_a_master_move(Board2, 'X'),
    {_Value, Board4} = tictactoe:take_a_master_move(Board3, 'O'),
    {_Value, Board5} = tictactoe:take_a_master_move(Board4, 'X'),
    {_Value, Board6} = tictactoe:take_a_master_move(Board5, 'O'),
    {_Value, Board7} = tictactoe:take_a_master_move(Board6, 'X'),
    {_Value, Board8} = tictactoe:take_a_master_move(Board7, 'O'),
    {_Value, Board9} = tictactoe:take_a_master_move(Board8, 'X'),
    [?_assert(tictactoe:is_terminal_state(Board9)),
     ?_assertEqual(draw, tictactoe:who_wins(Board9))].

%% In some situations, there is clearly one best movement. Check
%% that the algorithm actually makes the right choice.
make_obvious_choices_test_() ->
    Board1 =   ['E', 'O', 'E', 'E', 'O', 'E', 'X', 'E', 'X'],
    {_, NB1} = tictactoe:take_a_master_move(Board1, 'X'),

    Board2 =   ['X', 'X', 'E', 'O', 'E', 'E', 'E', 'O', 'E'],
    {_, NB2} = tictactoe:take_a_master_move(Board2, 'O'),

    Board3 =   ['X', 'E', 'X', 'O', 'O', 'E', 'E', 'O', 'E'],
    {_, NB3} = tictactoe:take_a_master_move(Board3, 'X'),

    Board4 =   ['X', 'O', 'X', 'X', 'O', 'E', 'E', 'E', 'E'],
    {_, NB4} = tictactoe:take_a_master_move(Board4, 'O'),

    [?_assertEqual(['E', 'O', 'E', 'E', 'O', 'E', 'X', 'X', 'X'], NB1),
     ?_assertEqual(['X', 'X', 'O', 'O', 'E', 'E', 'E', 'O', 'E'], NB2),
     ?_assertEqual(['X', 'X', 'X', 'O', 'O', 'E', 'E', 'O', 'E'], NB3),
     ?_assertEqual(['X', 'O', 'X', 'X', 'O', 'E', 'E', 'O', 'E'], NB4)].

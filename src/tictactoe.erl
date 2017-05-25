%%===================================================================
%% @doc Module that implements the minimax AI alrgorithm for TIC TAC TOE
%% gaming.
%% @end
%%===================================================================
-module(tictactoe).

-export([who_wins/1,
         minimax_value/2,
         take_a_master_move/2,
         is_terminal_state/1,
         moves_count/1]).

-define(STARTBOARD, ['E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E']).

-type player() :: 'X' | 'O'.

-type square() :: player() | 'E'.

%% Dialyzer does not check number of elements on lists types.
%% We could use a tuple, but for convenience prefer a list.
-type board() :: list(square()).

-type game_result() :: player() | 'draw' | 'running'.

% Two Players, X and O. E means empty.
% The board is a 9-element list

%%===================================================================
%% External functions
%%===================================================================

%% @doc Given a Board state (list) it will return who won if the game is
%% finished, if it is a draw or if it is still running
%% @end
-spec who_wins(Board :: board()) -> game_result().
%% Rows
who_wins([A, A, A, _, _, _, _, _, _]) when A == 'X' orelse A == 'O' -> A;
who_wins([_, _, _, A, A, A, _, _, _]) when A == 'X' orelse A == 'O' -> A;
who_wins([_, _, _, _, _, _, A, A, A]) when A == 'X' orelse A == 'O' -> A;
%% Columns
who_wins([A, _, _, A, _, _, A, _, _]) when A == 'X' orelse A == 'O' -> A;
who_wins([_, A, _, _, A, _, _, A, _]) when A == 'X' orelse A == 'O' -> A;
who_wins([_, _, A, _, _, A, _, _, A]) when A == 'X' orelse A == 'O' -> A;
%% diagonals
who_wins([A, _, _, _, A, _, _, _, A]) when A == 'X' orelse A == 'O' -> A;
who_wins([_, _, A, _, A, _, A, _, _]) when A == 'X' orelse A == 'O' -> A;
who_wins(Board) ->
    case lists:member('E', Board) of
        true ->
            running;
        false ->
            draw
    end.

%% @doc Given a board state (list), it will return the number of moves
%% that have happened since the game started. The minimum is 0 and the
%% maximum is 9 (the board is full).
%% @end
-spec moves_count(Board :: board()) -> integer().
moves_count(Board) ->
    9 - length(lists:filter(fun is_empty_square/1, Board)).

%% @doc Given a board state (list), it will return whether the game has
%% finished (somebody won or it is a draw) or it is still running (false).
%% @end
-spec is_terminal_state(Board :: board()) -> boolean().
is_terminal_state(Board) ->
    who_wins(Board) /= running.

%% @doc Given a representing a terminal state, it will score the result in
%% relation to the player who won or if it is a draw.
%% @end
-spec score_game(Board :: board()) -> integer() | {error, still_running}.
score_game(Board) ->
    case who_wins(Board) of
        'X' ->
            10 - moves_count(Board);
        'O' ->
            -10 + moves_count(Board);
        draw ->
            0;
        running ->
            {error, still_running}
    end.

%% @doc Given a board state and a player, it will return the minimax
%% integer value associated with it.
%% @end
-spec minimax_value(Board :: board(), Player :: player()) -> integer().
minimax_value(Board, Player) ->
    case is_terminal_state(Board) of
        true ->
            score_game(Board);
        false ->
            NextStates = all_possible_next_states(Board, Player),
            MinimaxValues = lists:map(opposite(Player), NextStates),
            case Player of
                'X' ->
                    lists:max(lists:flatten(MinimaxValues));
                'O' ->
                    lists:min(lists:flatten(MinimaxValues))
            end
    end.

%% @doc Given a board and a player, it will compute the best possible
%% move using the minimax algorithm. It will return the best possible
%% move (a board) and its associated minimax value in a tuple.
%% @end
-spec take_a_master_move(Board :: board(), Player :: player()) ->
    {integer(), board()}.
take_a_master_move(Board, Player) ->
    NextStates = all_possible_next_states(Board, Player),
    MinimaxValues = lists:map(opposite(Player), NextStates),
    StatesAndValues = lists:zip(MinimaxValues, NextStates),
    Sorted = lists:keysort(1, StatesAndValues),
    [ H | _T ] = case Player of
        'X' ->
            lists:reverse(Sorted);
        'O' ->
            Sorted
    end,
    H.

%%===================================================================
%% Internal Functions
%%===================================================================

maximize_value(Board) ->
    minimax_value(Board, 'X').

minimize_value(Board) ->
    minimax_value(Board, 'O').

opposite('X') ->
    fun minimize_value/1;
opposite('O') ->
    fun maximize_value/1.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

replace_empty_square(Board, Number, Player) ->
    setnth(Number, Board, Player).

-spec is_empty_square(Square :: square()) -> boolean().
is_empty_square('E') -> true;
is_empty_square(_) -> false.

all_possible_next_states(Board, Player) ->
    all_possible_next_states(Board, Board, [], Player).

all_possible_next_states(_Board, [], Acc, _Player) ->
    Acc;
all_possible_next_states(Board, [A | T], Acc, Player) when A == 'X' orelse A == 'O' ->
    all_possible_next_states(Board, T, Acc, Player);
all_possible_next_states(Board, ['E' | T], Acc, Player) ->
    NewAcc = [replace_empty_square(Board, 9 - length(T), Player)] ++ Acc,
    all_possible_next_states(Board, T, NewAcc, Player).

%%===================================================================
%% @doc This module implements a gen_server that holds the game state
%% for the Tic Tac Toe game. At the moment it implements a one player
%% mode, with the user always playing as player 'X' and the server
%% playing as player 'O'.
%% User (player 'X') is always the one who starts the game.
%% @end
%%===================================================================
-module(tictactoe_server).

-behaviour(gen_server).

-include("tictactoe.hrl").

%% external functions.
-export([start_link/0,
         reset_game/0,
         get_board_state/0,
         make_move/1,
         game_status/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        game_state = running :: tictactoe:game_result(),
        board = ?STARTBOARD :: tictactoe:board()
        }).

%%===================================================================
%% External functions
%%===================================================================

%% @doc Starts a locally registered instance of the game server.
%% To be called by a supervisor.
%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Resets the game to the initial state.
%% @end
-spec reset_game() -> ok.
reset_game() ->
    gen_server:call(?MODULE, reset_game).

%% @doc Returns the state of the game board at any time
%% @end
-spec get_board_state() -> {ok, tictactoe:board()}.
get_board_state() ->
    gen_server:call(?MODULE, get_board_state).

%% @doc Returns the state of the game at any point
%% @end
-spec game_status() -> {ok, tictactoe:game_result()}.
game_status() ->
    gen_server:call(?MODULE, get_game_status).

%% @doc It makes a move for player 'X'.
%% The server plays as player 'O', user is always player 'X'
%% The new board provided will be validated against the current board
%% for a move for player 'X' and the move will be performed if it is
%% valid.
%% The server will, straight away, make the next move for the player
%% 'O' and update the game state accordingly.
%% @end
-spec make_move(NewBoard :: tictactoe:board()) ->
    {ok, tictactoe:board()} | {error,  'invalid_move'}.
make_move(NewBoard) ->
    gen_server:call(?MODULE, {move, NewBoard}).

%%===================================================================
%% gen_server callbacks
%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(get_board_state, _From, #state{board = Board} = State) ->
    {reply, {ok, Board}, State};
handle_call(reset_game, _From, State) ->
    {reply, ok, State#state{board = ?STARTBOARD,
                            game_state = running}};
handle_call(get_game_status, _From, #state{game_state = GameState} = State) ->
    {reply, {ok, GameState}, State};
handle_call({move, NewBoard}, _From, #state{game_state = running,
                                                   board = Board} = State) ->
    {Res, NextBoard} = case tictactoe:is_valid_move(Board, NewBoard, 'X') of
        true ->
            B = get_next_board(NewBoard),
            {{ok, B}, B};
        false ->
            {{error, invalid_move}, Board}
    end,
    {reply, Res, State#state{
            game_state = tictactoe:who_wins(NextBoard),
            board = NextBoard}};
handle_call({move, _NB} , _From, #state{game_state = Status,
                                               board = Board} = State)
        when Status /= running ->
    {reply, {error, game_finished}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal Functions
%%===================================================================

get_next_board(NewBoard) ->
    case tictactoe:is_terminal_state(NewBoard) of
        false ->
            {_MV, B} = tictactoe:take_a_master_move(NewBoard, 'O'),
            B;
        true ->
            NewBoard
    end.

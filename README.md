Tic Tac Toe
========

This is an implementation of a Tic Tac Toe minimax AI algorithm in Erlang.

It provides a game board representation and a way to fetch optimal moves from different game positions

At the moment is just an OTP library, without supervisor tree of processes.

It is based on the Minimax algorithm described here: https://mostafa-samir.github.io/Tic-Tac-Toe-AI/

It also provides a game server that allows handling of the game status in memory.

Dependencies
-
It is expected to have Erlang/OTP installed and available in the $PATH. (version 17.4 +).
The easiest way to install Erlang/OTP is probably using Kerl: http://github.com/kerl/kerl

How to build and run
-
With:
```
$./rebar3 release
```
will compile the code and build a release. In order to start a release and an Erlang shell attached to it:

```
$ ./_build/default/rel/tictactoe/bin/tictactoe
```

Usage
-
The main function is `tictactoe:take_a_master_move/2`. It takes a board state, which is a 9 element list, and a player to play next. As an example:

```
1> tictactoe:take_a_master_move(['E','E','O','O','E','E','E','X','X'], 'X').
{5,['E','E','O','O','E','E','X','X','X']}
```

Detail API documentation is provided on `tictactoe.erl` module.

The game server holds the game status and can be accessed using its own API (tictactoe_server.erl).

```
1> tictactoe_server:game_status().
{ok,running}

2> tictactoe_server:get_board_state().
{ok,['E','E','E','E','E','E','E','E','E']}

3> tictactoe_server:make_move(['E','E','E','E','E','X','E','E','E']).
{ok,['E','E','E','E','E','X','E','E','O']}
```
The server plays as player `'O'` and the user plays as player `'X'`. The server will make the next move automatically after the user calls the `make_move/1` function to perform a move.

Running Dialyzer
-
The following command will run Dialyzer type checker:

```
$ ./rebar3 dialyzer
```

Running Eunit tests
-
Some simple Eunit tests that provide a good level of coverage are provided.

For running the Eunit tests:

```
$ ./rebar3 eunit
```

Generating coverage report
-
After running Eunit tests, the following command will generate an HTML coverage report:

```
$ ./rebar3 cover
```

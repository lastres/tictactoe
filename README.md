Tic Tac Toe
========

This is an implementation of a Tic Tac Toe minimax AI algorithm in Erlang. (so far).

It provides a game board representation and a way to fetch optimal moves from different game positions

At the moment is just an OTP library, without supervisor tree of processes.

It is based on the Minimax algorithm described here: https://mostafa-samir.github.io/Tic-Tac-Toe-AI/

Dependencies
-
It is expected to have Erlang/OTP installed and available in the $PATH. (version 17.4 +).
The easiest way to install Erlang/OTP is probably using Kerl: http://github.com/kerl/kerl

How to build and run
-
With:
```
$./rebar3 shell
```
will compile the code and start a shell with the library code loaded.

Usage
-
The main function is `tictactoe:take_a_master_move/2`. It takes a board state, which is a 9 element list, and a player to play next. As an example:

```
1> tictactoe:take_a_master_move(['E','E','O','O','E','E','E','X','X'], 'X').
{5,['E','E','O','O','E','E','X','X','X']}
```

Detail API documentation is provided on `tictactoe.erl` module.

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

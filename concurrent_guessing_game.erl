-module(concurrent_guessing_game).
-export([start/0, start/4]).

% Concurrent guessing game with Erlang.
% Compile with: `c(concurrent_guessing_game).`
% Can be started using default values with: `concurrent_guessing_game:start().`
% or using custom values with: `concurrent_guessing_game:start("Player 1", [1, 2, 3, 4], "Player 2", [6, 5, 4]).`

% Start the game with default values if no parameters were given
start() ->
    start("Player 1", [1, 2, 3, 4], "Player 2", [6, 5, 4]).

% Start the game with the two named players and their lists of integers
%       String,      [Int],       String,       [Int]
start(Player1Name, Player1List, Player2Name, Player2List) ->
    % Spawn two player processes
    Player1 = spawn(fun() -> player(Player1Name, self(), Player1List) end),
    Player2 = spawn(fun() -> player(Player2Name, self(), Player2List) end),

    timer:sleep(500),

    % Set the opponent for both players
    Player1 ! {set_opponent, Player2, Player2Name},
    Player2 ! {set_opponent, Player1, Player1Name},

    % Start the game with Player 1 process making the first guess
    Player1 ! start_game.

% The player process
player(Name, Opponent, List) ->
    receive
        % Starting the game
        start_game ->
            case List of
                [] ->
                    io:format("~s won~n", [Name]),
                    Opponent ! you_lose;
                [H | T] ->
                    io:format("~s guesses ~p~n", [Name, H]),
                    Opponent ! {guess, self(), H},
                    player(Name, Opponent, T)
            end;
        % Receiving a guess from the opponent
        {guess, Opponent, Guess} ->
            timer:sleep(500),
            case lists:member(Guess, List) of
                % The player loses if the opponent's guess is in their list
                true ->
                    io:format("~s lost~n", [Name]),
                    Opponent ! you_win;
                % Continue the game if the guess is not in their list
                false ->
                    case List of
                        [] ->
                            io:format("~s won~n", [Name]),
                            Opponent ! you_lose;
                        [H | T] ->
                            io:format("~s guesses ~p~n", [Name, H]),
                            Opponent ! {guess, self(), H},
                            player(Name, Opponent, T)
                    end
            end;
        % Set the opponent
        {set_opponent, OpponentId, OpponentName} ->
            io:format("~p is setting ~p as their opponent~n", [Name, OpponentName]),
            player(Name, OpponentId, List);
        % Receive a winning message
        you_win ->
            io:format("~s won~n", [Name]);
        % Receive a losing message
        you_lose ->
            io:format("~s lost~n", [Name])
    end.

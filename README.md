# concurrent-guessing-game-with-haskell-and-erlang

This repository contains a simple concurrent guessing game implemented with Haskell and Erlang as independent course exercises. The game creates two threads, each with a name and a list of integers `[Int]`.

- Threads take turns sending an integer from their list to the other thread.
- Sent integers are removed from the sender's list.
- The game ends when:
  - **Loss**: A thread receives an integer still in its own list.
  - **Win**: A thread runs out of integers to send.

## Running the applications

### Haskell version

Run the application using `ghci`:

```bash
# Load the file in GHCi
:l concurrent_guessing_game.hs

# Start the application with:
main

# Example input:
"Player 1"
[1, 2, 3, 4]
"Player 2"
[6, 5, 4]
```

### Erlang version

Run the app in erlang shell.

```bash
# Compile the module
c(concurrent_guessing_game).

# Start the system with default values:
concurrent_guessing_game:start().

# Or specify custom values, for example:
concurrent_guessing_game:start("Player 1", [1, 2, 3, 4], "Player 2", [6, 5, 4]).
```

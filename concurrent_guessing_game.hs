import Control.Concurrent
import System.IO

{-|
Concurrent guessing game with Haskell.
Compile with ghci using the command: `:l concurrent_guessing_game.hs`
Run the application by calling the main function and then giving 4 input lines.

Example input:
"Player 1"
[1, 2, 3, 4]
"Player 2"
[6, 5, 4]
-}

main = do
 input1 <- getLine
 input2 <- getLine
 input3 <- getLine
 input4 <- getLine
 mVal1 <- newEmptyMVar 
 mVal2 <- newEmptyMVar
 msg <- newEmptyMVar
-- parameters for start / guess are: name, value_in, value_out, message_to_other, int_list 
 forkIO (start (read input1::String) mVal1 mVal2 msg (read input2::[Int]))
 forkIO (guess (read input3::String) mVal2 mVal1 msg (read input4::[Int]))
 threadDelay 2000 -- without this the main thread will die and so will the forked threads

-- won already when starting because the list is empty
start :: String -> MVar Int -> MVar Int -> MVar String -> [Int] -> IO ()
start myName mVal1 mVal2 msg [] = do
 putMVar msg "I won"
 putMVar mVal2 0 -- has to put a value because thats what the player is reading
 putStrLn (myName ++ " won")

-- starting is different from other situations
start myName mVal1 mVal2 msg (i:is) = do
 putStrLn (myName ++ " guesses " ++ (show i))
 putMVar mVal2 i
 putMVar msg "your turn"
 guess myName mVal1 mVal2 msg is

-- the guess function handles taking turns between players
guess :: String -> MVar Int -> MVar Int -> MVar String -> [Int] -> IO ()
guess myName mVal1 mVal2 msg myList = do
  -- wait for a message from the other player
  takeMVar msg
  -- receive the guessed value from the other player
  theirGuess <- takeMVar mVal1
  -- check if the game should end (based on the current list of the other player)
  if theirGuess == 0
    then putStrLn (myName ++ " won") -- they have no more moves left, so we won
    else do
      if theirGuess == (-1)
        then putStrLn (myName ++ " lost")
        else do
          -- check if the opponent's guess is in our list
          if theirGuess `elem` myList
            then do
              putStrLn (myName ++ " lost") -- we lost because they guessed one of our numbers
              putMVar mVal2 0 -- send 0 to indicate game over to the other player
              putMVar msg "your turn"
            else do
              -- if we haven't lost, make our guess and continue
              case myList of
                [] -> do
                  putStrLn (myName ++ " won")
                  putMVar mVal2 (-1) -- send 0 to indicate we won and the game is over
                  putMVar msg "your turn"
                (x:xs) -> do
                  putStrLn (myName ++ " guesses " ++ show x)
                  putMVar mVal2 x -- send our guess to the other player
                  putMVar msg "your turn"
                  guess myName mVal1 mVal2 msg xs -- continue the game with the rest of the list
  
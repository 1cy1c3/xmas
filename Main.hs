module Main where

import Control.Concurrent(threadDelay)
import Data.Array
import System.IO

import State
import Game
import Cmd

{-
  Print the state.
-}
printState :: State -> IO ()
printState st = do
  -- tput clear, effective only on Linux
  putStrLn "\ESC[H\ESC[2J"
  putStrLn $ render st

{-
  Game main loop
-}
loop :: State -> IO ()
loop st = do
  printState st
  if length (presents st) == 0 then 
    putStrLn "Congratulations. You won!" 
  else 
    if (loose st) then putStrLn ("What a pity. You lost!")
    else do
      threadDelay 10000
      c <- getCmd
      case c of
        Move m -> loop (nextState m st) 
        Quit -> putStrLn "Goodbye!"

{-
  Main entry point
-}
main :: IO ()
main = do
  putStrLn "Setting up..."  
  -- This does nothing under windows.    
  hSetBuffering stdin NoBuffering
  i <- initialState 50 20
  loop i

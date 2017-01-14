{-# LANGUAGE CPP #-}
module Cmd(
   Cmd(..)
 , Move(..)
 , getCmd
 ) where

import System.IO

getKeystrokes :: IO String
#ifdef mingw32_HOST_OS
-- Under Windows, we can only read whole lines reliably.
getKeystrokes = do
  putStr "Your command> "; hFlush stdout
  getLine
#else
-- Under normal o/s's, we can read single chars.
getKeystrokes = do
  c <- getChar -- We wait for the first character to appear
  getMore [c] where -- Then we read more keystrokes (if available)
    getMore cs = do
      r <- hReady stdin
      if r then do c <- getChar
                   getMore (c: cs)
           else return $ reverse cs
#endif

-- Command keys
keyUp, keyRU, keyRi, keyRD, keyDo, keyLD, keyLe, keyLU, keyQuit :: String
#ifdef mingw32_HOST_OS
-- Simple commands for Windows
keyUp = "u" 
keyRU = "ru"
keyRi = "r"
keyRD = "rd"
keyDo = "d"
keyLD = "ld"
keyLe = "l" 
keyLU = "lu" 
keyQuit = "q" 
#else
-- VT100-like cursor keys for others
keyUp = "\ESC[A" 
keyRU = "\ESC[5~" -- ScrnUp
keyRi = "\ESC[C"
keyRD = "\ESC[6~" -- ScrnDn
keyDo = "\ESC[B"
keyLD = "\ESC[F"  -- End
keyLe = "\ESC[D" 
keyLU = "\ESC[H" -- Pos1
keyQuit = "q" 
#endif

data Cmd = Move Move | Quit  deriving Show

data Move = Up | RU | Ri | RD | Do | LD | Le | LU | Idle deriving Show

{-
  Translate a string to a command.
-}
key2cmd :: String -> Cmd
key2cmd s
  | s == keyUp = Move Up
  | s == keyRU = Move RU 
  | s == keyRi = Move Ri
  | s == keyRD = Move RD 
  | s == keyDo = Move Do 
  | s == keyLD = Move LD 
  | s == keyLe = Move Le
  | s == keyLU = Move LU 
  | s == keyQuit = Quit
  | otherwise = Move Idle

getCmd :: IO Cmd
getCmd = fmap key2cmd getKeystrokes

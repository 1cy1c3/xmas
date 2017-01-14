module Game(
  nextState
  ) where

import Data.Array
import Cmd
import State
import Config

{-
  Move the player.
-}
moveToPos :: Move -> State -> Maybe Pos
moveToPos m st =
  let p' = case (m, self st) of
        (Up, (x, y)) -> (x, y-1)
        (RU, (x, y)) -> (x+1, y-1)
        (Ri, (x, y)) -> (x+1, y)
        (RD, (x, y)) -> (x+1, y+1)
        (Do, (x, y)) -> (x, y+ 1)
        (LD, (x, y)) -> (x-1, y+1)
        (Le, (x, y)) -> (x-1, y)
        (LU, (x, y)) -> (x-1, y-1)
        (Idle, (x, y)) -> (x, y)
  in if validPos st p' then Just p' else Nothing

{-
  Calculate the next state.
-}
nextState :: Move -> State -> State
nextState m st = 
  incMov $ case moveToPos m st of
    Nothing -> playerIsEaten(moveWolvesState(checkPlayerIsOnPresent 
      (isPresentInRange (isWolfInRange st ))))
    Just p | not (fields st ! p) -> playerIsEaten(moveWolvesState
      (checkPlayerIsOnPresent (isPresentInRange (isWolfInRange st {self= p }))))
           | otherwise -> st -- hit an obstacle
  where
    {-
      Check whether a wolf is in range.
    -}
    isWolfInRange :: State -> State
    isWolfInRange st = st {wolves = checkAndSetVisible (wolves st) wolfVisibleRange (self st)}

    {-
      Check whether a present is in range.
    -}
    isPresentInRange :: State -> State
    isPresentInRange st = st {presents = checkAndSetVisible (presents st) 
      presentVisibleRange (self st)}

    {-
      Check whether a player is eaten.
    -}
    playerIsEaten :: State -> State
    playerIsEaten st 
      | existsAtPos (self st) (wolves st) = st {loose = True}
      | otherwise = st

    {-
      Move wolves.
    -}
    moveWolvesState :: State -> State
    moveWolvesState st = st {wolves = (moveWolves (wolves st) [] (self st) st)}
    
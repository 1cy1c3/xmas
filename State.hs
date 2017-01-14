module State where

import Data.Array
import System.Random
import Config

type Pos = (Int, Int)

{-
  Is the position valid?
-}
validPos :: State -> Pos -> Bool
validPos st (x, y) = 0 <= x && x <= xMax st && 
  0 <= y && y <= yMax st

-- The game state with fields, wolves and so on.
data State = St {   fields :: Array Pos Bool
                  , self   :: Pos
                  , moves  :: Int
                  , wolves :: [(Pos, Bool, Int)]
                  , presents :: [(Pos, Bool, Int)]
                  , score :: Int
                  , loose :: Bool
                }

{-
  Maximum x position.
-}
xMax :: State -> Int
xMax = fst . snd . bounds . fields

{-
  Maximum y position.
-}
yMax :: State -> Int
yMax = snd . snd . bounds . fields

{-
  Increment the move counter.
-}
incMov :: State -> State
incMov st = st {moves = moves st+1}

{-
  Create an initial state.
-}
initialState :: Int -> Int -> IO State
initialState x y = do
  rndx <- mapM randomRIO (replicate (numTrees (x*y)+numPresents) (0, x))
  rndy <- mapM randomRIO (replicate (numTrees (x*y)+numPresents) (0, y))  
  rndz <- mapM randomRIO (replicate (numTrees (x*y)+numPresents) (10, 20::Int)) 
  let mt = array ((0, 0), (x, y)) [((i, j), False) | i<- [0.. x], j<- [0.. y]]
      zipper = zip3 rndx rndy rndz
      fieldsOfTree = [((x, y), True) | (x, y,_) <- take (numTrees (x*y)) zipper ]
      flds = mt // fieldsOfTree
      self = (x `div` 2, y `div` 2)
      presents = [((x, y), False, z) | (x, y,z) <- lastN numPresents zipper ]
      wolves = initWolves fieldsOfTree (numWolves (x*y)) (St flds self 0 [] presents 0 False)
  return $ St flds self 0 wolves presents 0 False
  where
    lastN :: Int -> [a] -> [a]
    lastN n xs = drop (length xs - n) xs

{-
  Check whether a wolve or present is visible.
-}
isVisible :: Pos -> [(Pos, Bool, Int)] -> Bool
isVisible p [] = False
isVisible p ((x,b,i):xs) 
  | p == x = b
  | otherwise = isVisible p xs
  
{-
  Check whether a wolve or present exists at the position.
-}
existsAtPos :: Pos -> [(Pos, Bool, Int)] -> Bool
existsAtPos p [] = False
existsAtPos p ((x,b,i):xs) 
  | p == x = True
  | otherwise = existsAtPos p xs

{-
  Initialize wolves in this game.
-}
initWolves :: [(Pos, Bool)] -> Int -> State -> [(Pos, Bool, Int)]
initWolves _ 0 _ = []
initWolves (((x,y),b):xs) n st = ((x,y), False, 0) : initWolves xs (n-1) st
  
{-
  Move the wolves in the direction of the player.
-}
moveWolves :: [(Pos, Bool, Int)] -> [(Pos, Bool, Int)] -> Pos -> State -> [(Pos, Bool, Int)]
moveWolves [] l _ _ = l
moveWolves (((x,y),b,i):xs) l (x',y') st
  | not b = (moveWolves xs (((x,y),b,i) : l) (x',y') st)
  | not ((i+1) `mod` wolvesMoverFactor == 0) = (moveWolves xs (((x,y),b,i+1) : l) (x',y') st)
  | x < x' && not (collisionWithWolf st (l ++ xs) (x+1,y)) = 
    (moveWolves xs (((x+1,y),b,i+1) : l) (x',y') st)
  | x > x' && not (collisionWithWolf st (l ++ xs) (x-1,y)) = 
    (moveWolves xs (((x-1,y),b,i+1) : l) (x',y') st)
  | y < y' && not (collisionWithWolf st (l ++ xs) (x,y+1)) = 
    (moveWolves xs (((x,y+1),b,i+1) : l) (x',y') st)
  | y > y' && not (collisionWithWolf st (l ++ xs) (x,y-1)) = 
    (moveWolves xs (((x,y-1),b,i+1) : l) (x',y') st)
  | otherwise = (moveWolves xs (((x,y),b,i) : l) (x',y') st)
  where
    {-
      Is there a collision with a wolf?
    -}
    collisionWithWolf :: State -> [(Pos, Bool, Int)] -> Pos -> Bool
    collisionWithWolf st l p = existsAtPos p l || fields st ! p 

{-
  Set a present or wolf as visible.
-}
checkAndSetVisible :: [(Pos, Bool, Int)] -> Int -> Pos -> [(Pos, Bool, Int)]
checkAndSetVisible [] _ _ = []
checkAndSetVisible ((x,b,i):xs) n p 
  | dist x p <= n = ((x,True,i) : checkAndSetVisible xs n p)
  | otherwise = ((x,b,i) : checkAndSetVisible xs n p)
  where
    {-
      Calculate the distance regarding coordinates.
    -}
    dist :: Pos -> Pos -> Int
    dist (x,y) (x',y') = abs (x - x') + abs (y - y')
  
{-
  Check whether a player is on a present field.
-}
checkPlayerIsOnPresent :: State -> State
checkPlayerIsOnPresent st = st {presents = p, score = ((score st) + s)} 
  where
    (p,s) = foldr (\(x,b,i) (p', s') -> 
      if (self st) == x 
        then (p',s' + i) 
      else 
        ((x,b,i):p',s')) ([], 0) (presents st)

{-
  Render the state.
-}
render :: State -> String
render st =
  let line i = '|': [renderField st (x, i) | x<- [0.. xMax st]] ++ "|\n"
      brdr   = '+' : replicate (xMax st+ 1) '-' ++ "+\n"
      stln   = "Points: " ++ show (score st) ++ "   Move: "++ show (moves st) ++ 
        "   Presents left: " ++ show ( length (presents st))
  in brdr ++ concatMap line [0.. yMax st] ++ brdr ++ stln
  where
    {-
      Render the field.
    -}
    renderField :: State -> Pos -> Char
    renderField st p
      | fields st ! p = '^'
      | isVisible p (wolves st) = 'W'
      | isVisible p (presents st) = '*'
      | p == self st = '@'
      | otherwise = ' '

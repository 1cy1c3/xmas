module Config where

-- Number of wolves
numWolves :: Int -> Int
numWolves n = n `div` 50

-- Number of trees
numTrees :: Int -> Int
numTrees n = n `div` 20

-- Number of presents
numPresents :: Int
numPresents = 5

-- Speed of wolves
wolvesMoverFactor :: Int
wolvesMoverFactor = 2

-- When are wolves visible?
wolfVisibleRange :: Int
wolfVisibleRange = 5

-- When are presents visible?
presentVisibleRange :: Int
presentVisibleRange = 3

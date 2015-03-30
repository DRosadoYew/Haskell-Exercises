{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = sum $ zipWith (\x y -> if x == y then 1 else 0) xs ys


-- Exercise 2 -----------------------------------------

-- For each Peg in colors, count how many times it occurs in xs.
-- Output should always have length 6 and the sum of all the entries 
-- should be equal to length Code.
countColors :: Code -> [Int]
countColors xs = map f colors
	where f x = length $ filter (\y -> y == x) xs

-- Count number of matches between the actual code and the guess
-- xs: actual code
-- ys: guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith (min) (countColors xs) (countColors ys)


-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
-- The first Code is the secret, the second Code is guess, and the output
-- is the resulting Move
getMove :: Code -> Code -> Move
getMove xs ys = Move ys a b
			where 
				a = exactMatches xs ys
				b = matches xs ys - a


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c a b) s = getMove s c == Move c a b


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m xs = filter (isConsistent m) xs


-- Exercise 6 -----------------------------------------

-- from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [Peg] -> [Code]
combinations k ns = filter ((k==).length) (subsequences ns)

allCodes :: Int -> [Code]
allCodes n = concat $ map (permutations) $ combinations n colors


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

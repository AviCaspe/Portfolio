{-# OPTIONS_GHC -Wall #-}

module Day24 where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
-- Essentially a tuple of two values
import Linear.V2
-- Essentially a tuple of three values
import Linear.V3
import qualified Data.Set as S

-- In this solution I use a Set to store the position of living bugs
-- I use (V2 x y) as the coordinates in part 1
-- I use (V3 x y z) as the coordinates in part 2
-- nextState is polymorphic and works for both parts

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let sol = mkGrid input -- Turn the input string into a Set (V2 Int)
          & iterate nextState -- Make an infinite list of all future states
          & firstDup -- Find the first duplicate in this list, or return Nothing if there is no dup
          & fromJust -- Assume a duplicate exist
          & biodiversity -- Finally calculate the biodiversity of the dup.
  print sol -- prints 28903899 for my input

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let sol = mkGrid input -- Turn input string into a Set (V2 Int)
          & S.map (\(V2 x y) -> V3 x y 0) -- State that the current layer is layer 0
          & iterate nextState -- Make an infinite list of all future states
          & (!! 200) -- Take the 200th element
          & S.size -- Count the number of live bugs
  print sol -- prints 1896 for my input

-- Advance time by one minute
nextState :: (Point p, Ord p) => S.Set p -> S.Set p
nextState bugs = S.filter update toEval
  where
    -- Get a Set of all the positions of bugs currently alive
    -- plus all the positions adjacent to alive bugs
    toEval = S.union bugs
           $ S.fromList
           $ concatMap neighbors
           $ S.toList bugs
    -- update returns true if this space will have a bug in a minute
    update p = if p `S.member` bugs
               then nBugs == 1
               else nBugs == 1 || nBugs == 2
       where
         -- neighbors p returns a list of neighbors for p
         -- then we check which of those squares contain live bugs
         nBugs = countOf (`S.member` bugs) $ neighbors p

-- Calculate Biodiversity
biodiversity :: S.Set (V2 Int) -> Int
biodiversity = sum . S.map (\(V2 x y) -> 2^(5*y + x))

-- Make a Set of points where the bugs start
mkGrid :: String -> S.Set (V2 Int)
mkGrid i = S.fromList [c | (c, k) <- pixels, k == '#']
  where
    pixels = [(V2 x y, z) | (y, row) <- zip [0..] rows, (x, z) <- zip [0..] row]
    rows = splitOn "\n" $ init i

-- Either returns the first duplicate value, or Nothing
firstDup :: (Foldable t, Ord a) => t a -> Maybe a
firstDup = either Just (const Nothing) . foldM go S.empty
  where
    go xs x
      | x `S.member` xs = Left x
      | otherwise = Right $ S.insert x xs

-- Count the number of elements in a foldable value that satisfy a predicate.
countOf :: Foldable t => (a -> Bool) -> t a -> Int
countOf p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

-- Used to for calculating adjacency
class Point p where
  neighbors :: p -> [p]

-- This is adjacency for part 1
instance (Num n, Ord n, Enum n) => Point (V2 n) where
  neighbors p = [p + d | d <- [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0], inRange (p + d)]
    where
      inRange (V2 x y) = 0 <= x && x <= 4
                      && 0 <= y && y <= 4

-- This is adjacency for part 2
instance (Num n, Ord n, Enum n) => Point (V3 n) where
  neighbors c = concat [up c, down c, left c, right c]
    where
      up (V3 _ 0 z) = [V3 2 1 (z+1)]
      up (V3 2 3 z) = [V3 x 4 (z-1) | x <- [0..4]]
      up (V3 x y z) = [V3 x (y-1) z]
      down (V3 _ 4 z) = [V3 2 3 (z+1)]
      down (V3 2 1 z) = [V3 x 0 (z-1) | x <- [0..4]]
      down (V3 x y z) = [V3 x (y+1) z]
      left (V3 0 _ z) = [V3 1 2 (z+1)]
      left (V3 3 2 z) = [V3 4 y (z-1) | y <- [0..4]]
      left (V3 x y z) = [V3 (x-1) y z]
      right (V3 4 _ z) = [V3 3 2 (z+1)]
      right (V3 1 2 z) = [V3 0 y (z-1) | y <- [0..4]]
      right (V3 x y z) = [V3 (x+1) y z]

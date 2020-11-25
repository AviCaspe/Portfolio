{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Day20 where

import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
-- Essential just a tuple of two values
import Linear.V2
-- A standard key-value map
import qualified Data.Map as M

-- Solution is 632 for my data
part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  print $ solve (const 0) -- No layer changes ever happen

-- Solution is 7162 for my data
part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  print $ solve delta
  where
    -- The if statement checks if a portal entrance
    -- is on the outer ring or not
    delta (V2 x y) = if x == 2 || x == 126 || y == 2 || y == 120
                     then (-1) -- Outer ring
                     else 1    -- Inner ring

-- Common code for both parts
-- Layer Change tells us how much the layer changes
-- When going through a portal at a certain location
solve :: (Pos -> Int) -- Layer Change function
      -> String       -- Input
      -> Int          -- Solution
solve delta i = steps
              $ fromJust
              -- End condition
              $ find (\as -> location as == end && layer as == 0)
              $ bfs (\as -> (location as, layer as)) -- State representation
                      (stepOnce maze portals delta)  -- State Change
                      (mkSS start)                   -- Starting State
  where
    start = snd . fromJust $ find ((== "AA") . fst) labels
    end = snd . fromJust $ find ((== "ZZ") . fst) labels
    portals = mkConnections labels
    labels = findPortals maze
    maze = mkMaze i

type Maze = M.Map (V2 Int) Char
type Pos = V2 Int
data AState = AS
  { steps    :: !Int  -- Steps taken
  , layer    :: !Int  -- Current Layer
  , location :: !Pos  -- Current Location
  } deriving (Show, Ord, Eq)

-- Make the initial Search State
mkSS :: V2 Int -> AState
mkSS = AS 0 0

-- Layer Change tells us how much the layer changes
-- When going through a portal at a certain location
stepOnce :: Maze          -- Maze
         -> M.Map Pos Pos -- Portal Connections
         -> (Pos -> Int)  -- Layer Change function
         -> AState        -- Starting State
         -> [AState]      -- Reachable States
stepOnce maze portals delta AS{..} = do
  nextLoc <- adjacent location
  case at nextLoc of
    '#' -> [] -- Wall
    '.' -> [AS (steps + 1) layer nextLoc] -- Floor
    a | isUpper a -> -- Portal
        case M.lookup location portals of
          Nothing -> [] -- This portal leads nowhere
          Just exitLoc -> [AS (steps + 1) newLayer exitLoc
                          | newLayer <- [layer + delta location]
                          , newLayer >= 0] -- Can't go to negative layers
    _ -> error "Invalid Location"

  where
    at p = M.findWithDefault '#' p maze

-- BFS Search on a single starting state
bfs :: Ord b
    => (a -> b)   -- State Repersentation What parts of this state make it different from the others?
    -> (a -> [a]) -- Next States
    ->  a         -- Initial State
    -> [a]        -- Reachable States (might be infinite)
bfs rep next start = go S.empty [start] []
  where
    go _    [] [] = []
    go seen [] ys = go seen (reverse ys) []
    go seen (x:xs) ys
      | S.member r seen = go seen xs ys
      | otherwise = x : go (S.insert r seen) xs (next x ++ ys)
      where r = rep x

-- All the positions adjacent to the p
adjacent :: V2 Int -> [V2 Int]
adjacent p = fmap ($ p) [up, down, left, right]

-- Adjacency functions
up, down, left, right :: V2 Int -> V2 Int
up    p = V2   0  (-1) + p
down  p = V2   0    1  + p
left  p = V2 (-1)   0  + p
right p = V2   1    0  + p

-- This finds all the portal entrances
-- and their locations. Each ID shows up
-- twice in the output list. (Each ID has an inner and an outer portal)
findPortals :: Maze -> [(String, Pos)]
findPortals maze = ends
  where
    ends = [x | (k, v) <- M.toList maze
            , isAlpha v
            -- All 4 ways of having a portal ID
            , x <- [([at (left k), at k] , right k) | isAlpha (at (left  k)), '.' == at (right k)] ++
              [([at k, at (down k)], up k) | isAlpha (at (down k)), '.' == at (up k)] ++
              [([at (up k), at k], down k) | isAlpha (at (up k)), '.' == at (down k)] ++
              [([at k, at (right k)], left k)  | isAlpha (at (right k)), '.' == at (left k)]]
    at p = M.findWithDefault '#' p maze

-- Take a list of portal IDs and locations
-- And make a map of connected locations
mkConnections :: [(String, Pos)] -> M.Map Pos Pos
mkConnections ends = M.fromList $
                     concatMap mkEntry $
                     M.elems $
                     M.fromListWith (++)
                     [ (k,[v]) | (k, v) <- ends
                              , k /= "AA" -- Filter out start and end
                              , k /= "ZZ"]
  where
    mkEntry [p1,p2] = [(p1,p2),(p2,p1)]
    mkEntry _ = []

mkMaze :: String -> Maze
mkMaze = M.filter (/= ' ')
       . createGrid
       . splitOn "\n"
       . init

-- Turn a list of strings into
-- a Map storing coords as keys
-- and characters as values
createGrid :: [[Char]] -> Maze
createGrid chars = M.fromList labelledCells
  where
    labelledRows = zip [0..] chars
    labelledCells = map (\(y,row) -> zipWith (\x char -> (V2 x y, char)) [0..] row) labelledRows

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Day22 where

import Data.Functor
import Data.Semigroup
-- Because I'm using the modular-arithemtic,
-- all mathematical operations will be modular
import Data.Modular
import GHC.TypeLits hiding (Mod)
-- From the Megaparsec library
import Text.Megaparsec

-- Used for parsing
data ShuffleOp = NewDeal
               | Cut Integer
               | Increment Integer
               deriving (Show, Eq)

-- This represents a series of shuffle operations
-- It's stores two integers n and m
-- It represents a mathematical function like so:
-- newPosition = n * oldPosition + m
-- Here func means a mathematical function
data ShuffleFunc s = SF (Integer `Mod` s) (Integer `Mod` s) deriving Show

-- Returns 6794 for my input
part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let = SF n m = mkShuffleFunc @10007 i
  print (unMod $ n * 2019 + m)

-- Returns 70725194521472 for my input
part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  -- This tells us how to apply all the shuffle operations once
  let once = mkShuffleFunc @119315717514047 i
  -- stimes is a built-in Haskell function that really does
  -- figure out what one linear function represents doing the shuffle operation
  -- that many times in a row
  -- It only works because the Semigroup definition below tells Haskell
  -- how to apply two shuffles in order
  let (SF n m) = stimes (101741582076661::Integer) once
  -- This is just the inverse of the newPos = n * oldPos + m
  -- linear function that stimes created for us.
  print (unMod $ (2020 - m) `div` n)

-- Creates a shuffle function
-- This takes the input data, and turns it into
-- A single linear function representing
-- applying all the operations in the input once
mkShuffleFunc :: (KnownNat s)  -- Witness that s is a nat, required for the Modular Arithmetic
              => String        -- Input
              -> ShuffleFunc s -- Linear Function
mkShuffleFunc i = foldMap shuffle ops
  where
    shuffle NewDeal       = SF (-1) (- 1)
    shuffle (Cut n)       = SF 1 (toMod $ -n)
    shuffle (Increment n) = SF (toMod n) 0
    ops = parseShuffle i

-- Applying ShuffleFunc x, then ShuffleFunc y is the same as
-- just Shuffling once, using a combined function
instance (KnownNat s) => Semigroup (ShuffleFunc s) where
  -- c(ax + b) + d
  -- cax + cb + d
  SF a b <> SF c d = SF (c * a) (c * b + d)

-- Monoid requires us to add be able to create an identity element
-- Here the Identity is to do nothing to the deck's order
instance (KnownNat s) => Monoid (ShuffleFunc s) where
  -- 1x + 0
  mempty = SF 1 0

-- Parsing the input with Megaparsec follows
-- This will crash with an error if the parser
-- isn't written correctly.
parseShuffle :: String -> [ShuffleOp]
parseShuffle = coerceParseResult . getParsedLines parseOps
  where
    parseOps = try ("deal into new stack" $> NewDeal)
               <|> try parseCut <|> parseIncrement
    parseCut = do
      n <- "cut " *> number
      return $ Cut $ fromIntegral n
    parseIncrement = do
      n <- "deal with increment " *> number
      return $ Increment $ fromIntegral n

getParsedLines :: Parser a -> String -> ParserResult [a]
getParsedLines p input = parseInput input (p `endBy` char '\n')

coerceParseResult :: ParserResult a -> a
coerceParseResult = either (error . show) id

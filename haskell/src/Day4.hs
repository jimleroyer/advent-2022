{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( contains,
    intoPair,
    intoPairs,
    overlaps,
    part1,
    part2,
    solve1,
    solve2,
  )
where

import Data.List (intersect, isInfixOf, isSubsequenceOf)
import Data.List.Split (splitOn)
import System.IO ()

filename :: FilePath
filename = "./data/day04-input.txt"

type Pair = (Int, Int)

type Pairs = (Pair, Pair)

type Range = [Int]

intoPair :: String -> Pair
intoPair strPair = (first, second)
  where
    parsed = splitOn "-" strPair
    first = read (head parsed) :: Int
    second = read (last parsed) :: Int

intoPairs :: String -> Pairs
intoPairs strPairs = (first, second)
  where
    parsed = splitOn "," strPairs
    first = intoPair (head parsed)
    second = intoPair (last parsed)

checks :: (Range -> Range -> Bool) -> Pairs -> Bool
checks fn pairs = fn crossed range1 || fn crossed range2
  where
    intoRange (first, second) = [first .. second]
    range1 = intoRange (fst pairs)
    range2 = intoRange (snd pairs)
    crossed = range1 `intersect` range2

contains :: Pairs -> Bool
contains = checks (==)

overlaps :: Pairs -> Bool
overlaps = checks (\x _ -> not (null x))

solve1 :: [Pairs] -> Int
solve1 pairs = sum (map (fromEnum . contains) pairs)

solve2 :: [Pairs] -> Int
solve2 pairs = sum (map (fromEnum . overlaps) pairs)

part1 :: IO ()
part1 = do
  pairs <- map intoPairs . lines <$> readFile filename
  let count = solve1 pairs
  print count

part2 :: IO ()
part2 = do
  pairs <- map intoPairs . lines <$> readFile filename
  let count = solve2 pairs
  print count

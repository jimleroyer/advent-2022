{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( part1,
    part2,
    halfSplit,
    mkUniq,
    priority,
    sameInCompartment,
    sameInBags,
    solve1,
    solve2,
  )
where

import Data.Char (ord, toUpper)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, toList)
import System.IO ()

filename :: FilePath
filename = "./data/day03-input.txt"

type Item = Char

type Items = String

type Bag = Items

type Bags = [Items]

type Priority = Int

priority :: Item -> Priority
priority item
  | item < 'a' = ord item - ord 'A' + 27
  | otherwise = ord item - ord 'a' + 1

halfSplit :: [a] -> ([a], [a])
halfSplit list = splitAt (length list `div` 2) list

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

sameInCompartment :: Bag -> Items
sameInCompartment items = mkUniq (uncurry intersect (halfSplit items))

sameInBags :: Bags -> Items
sameInBags (x : xs) = mkUniq (foldr intersect x xs)
sameInBags _ = []

solve1 :: Bag -> Int
solve1 bag = sum (map priority (sameInCompartment bag))

solve2 :: Bags -> Int
solve2 bags = sum (map priority (sameInBags bags))

part1 :: IO ()
part1 = do
  bag <- lines <$> readFile filename
  print (sum (map solve1 bag))

part2 :: IO ()
part2 = do
  allBags <- lines <$> readFile filename
  let groupedBags = chunksOf 3 allBags
  print (sum (map solve2 groupedBags))

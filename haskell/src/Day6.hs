{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6
  ( detect,
    solve1,
    solve2,
    main,
  )
where

import Data.List (tails)
import Data.Set (Set, fromList, toList)
import System.IO ()
import Text.Printf (printf)

filename :: FilePath
filename = "./data/day06-input.txt"

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

detect :: Int -> [[Char]] -> Int -> Int
detect n (x : xs) i
  | length (mkUniq x) == n = i + n
  | otherwise = detect n xs (i + 1)

solve :: Int -> [Char] -> Int
solve n signal = detect n (windows n signal) 0

solve1 :: [Char] -> Int
solve1 = solve 4

solve2 :: [Char] -> Int
solve2 = solve 14

main :: IO ()
main = do
  line <- head . lines <$> readFile filename
  let posMarker4 = solve1 line
  let posMarker14 = solve2 line
  printf "Part 1: %d, Part 2: %d" posMarker4 posMarker14

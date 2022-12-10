{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day8
  ( Grid (..),
    Tree (..),
    main,
    solve1,
    solve2,
    east,
    height,
    intoGrid,
    north,
    scenicScore,
    scenicViews,
    south,
    visibleTrees,
    west,
  )
where

import Data.Char (digitToInt)
import Data.List (find, inits, sort, tails, takeWhile)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Void
import System.IO ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

inputFilename :: FilePath
inputFilename = "./data/day08-input.txt"

type Tree = (Int, Int)

type Grid = [[Int]]

safeMax :: Ord a => a -> [a] -> a
safeMax default' list = maybe default' maximum (nonEmpty list)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

safeLast :: a -> [a] -> a
safeLast default' [] = default'
safeLast default' [x] = x
safeLast default' (_ : xs) = last xs

intoGrid :: [String] -> Grid
intoGrid = map (map digitToInt)

height :: Grid -> Tree -> Int
height grid (x, y) = (grid !! y) !! x

north :: Grid -> Tree -> [Tree]
north grid (x, y) = map (x,) [y - 1, y - 2 .. 0]

south :: Grid -> Tree -> [Tree]
south grid (x, y) = map (x,) [y + 1 .. border]
  where
    border = length grid - 1

west :: Grid -> Tree -> [Tree]
west grid (x, y) = map (,y) [x - 1, x - 2 .. 0]

east :: Grid -> Tree -> [Tree]
east grid (x, y) = map (,y) [x + 1 .. border]
  where
    border = length (head grid) - 1

cardinalities :: [Grid -> Tree -> [Tree]]
cardinalities = [north, south, east, west]

visibleTrees :: Grid -> [Tree]
visibleTrees grid = concatMap visible coords
  where
    coords = [(x, y) | y <- [1 .. length grid - 2], x <- [1 .. length (head grid) - 2]]
    getHeight = height grid
    getHeights tree cardinality = map getHeight (cardinality grid tree)
    directionalHeights tree = map (getHeights tree) cardinalities
    smallerThan tree = (< getHeight tree)
    visible tree = [tree | any (smallerThan tree . safeMax 0) (directionalHeights tree)]

scenicScore :: Grid -> Tree -> Int
scenicScore grid tree = scoreView tree
  where
    getHeight = height grid
    peek cardinality tree = maybeToList (safeHead (cardinality grid tree))
    getSmallerTrees myTree = takeWhile (\t -> getHeight t < getHeight myTree)
    getVisibleTrees myTree cardinality = visibleTrees ++ borderTree
      where
        visibleTrees = getSmallerTrees myTree (cardinality grid myTree)
        borderTree = peek cardinality (safeLast myTree visibleTrees)
    scoreDirection myTree cardinality = length (getVisibleTrees myTree cardinality)
    scoreView tree = product (map (scoreDirection tree) cardinalities)

scenicViews :: Grid -> [Int]
scenicViews grid = map (scenicScore grid) coords
  where
    coords = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (head grid) - 1]]

solve1 :: Grid -> Int
solve1 grid = length (visibleTrees grid) + borders
  where
    borders = length (head grid) * 2 + (length grid - 2) * 2

solve2 :: Grid -> Int
solve2 grid = maximum (scenicViews grid)

main :: IO ()
main = do
  ls <- lines <$> readFile inputFilename
  let grid = intoGrid ls
  let totalVisibles = solve1 grid
  let maxScenicScore = solve2 grid
  printf "Part 1: %d, Part 2: %d" totalVisibles maxScenicScore

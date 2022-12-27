{-# LANGUAGE OverloadedStrings #-}

module Day9
  ( main,
    solve1,
    solve2,
    Cell (..),
    Direction (..),
    mkUniq,
    move,
    moveHead,
    moveTail,
    parseLine,
    shouldMove,
  )
where

import Data.Char (digitToInt)
import Data.Either hiding (Left, Right)
import Data.List (find, inits, sort, splitAt, tails, takeWhile)
import Data.List.Split (chunksOf, splitOn)
import Data.Set (Set, fromList, toList)
import Data.Void
import System.IO ()
import Text.Printf (printf)
import Text.RawString.QQ (r)
import Prelude hiding (Left, Right)

inputFilename :: FilePath
inputFilename = "./data/day09-input.txt"

type Cell = (Int, Int)

-- data Knots = Knots {knotHead :: Cell, knotTail :: Cell} deriving (Eq, Show)

data Direction = Up | Down | Left | Right deriving (Eq, Show)

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right

parseLine :: String -> [Direction]
parseLine line = replicate times dir
  where
    [cDir, cTimes] = splitOn " " line
    (dir, times) = (parseDirection (head cDir), read cTimes :: Int)

shouldMove :: Cell -> Cell -> Bool
shouldMove (hx, hy) (tx, ty)
  | hx == tx || hy == ty = abs (hx - tx) + abs (hy - ty) > 1
  | otherwise = abs (hx - tx) + abs (hy - ty) > 2

moveHead :: Direction -> Cell -> Cell
moveHead direction (x, y) = case direction of
  Up -> (x, y + 1)
  Down -> (x, y - 1)
  Left -> (x - 1, y)
  Right -> (x + 1, y)

moveTail :: Direction -> Cell -> Cell -> Cell
moveTail direction head@(hx, hy) tail@(tx, ty)
  | shouldMove head tail = case direction of
      Up -> (hx, ty + 1)
      Down -> (hx, ty - 1)
      Left -> (tx - 1, hy)
      Right -> (tx + 1, hy)
  | otherwise = tail

move :: (Cell, Cell) -> Direction -> (Cell, Cell)
move (head@(hx, hy), tail@(tx, ty)) direction = (nextHead, nextTail)
  where
    nextHead = moveHead direction head
    nextTail = moveTail direction nextHead tail

solve1 :: [Direction] -> Int
solve1 directions = length visitedCells
  where
    rope0 = ((0, 0), (0, 0))
    ropes = foldl (\acc d -> move (head acc) d : acc) [rope0] directions
    visitedCells = mkUniq $ map snd ropes

solve2 :: [Direction] -> Int
solve2 directions = length visitedCells
  where
    moveRope direction rope =
      reverse $
        snd $
          foldl
            (\(remaining, new) kHead -> (tail remaining, moveTail direction (head new) (head remaining) : new))
            (rope, [moveHead direction (head rope)])
            rope
    collectRopes (ropes, tails) direction = (newRope : ropes, newTail : tails)
      where
        newRope = moveRope direction (head ropes)
        newTail = last newRope
    rope0 = replicate 10 ((0, 0) :: (Int, Int))
    init = ([rope0], [last rope0])
    (_, tails) = foldl collectRopes init directions
    visitedCells = mkUniq tails

main :: IO ()
main = do
  ls <- lines <$> readFile inputFilename
  let moves = concatMap parseLine ls
  let totalOccupedCells1 = solve1 moves
  -- printf "Part 1: %d" totalOccupedCells1
  let totalOccupedCells2 = solve2 moves
  printf "Part 1: %d, Part 2: %d" totalOccupedCells1 totalOccupedCells2

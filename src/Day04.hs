module Day04 where

import Data.List
import Data.List.Split
import Data.Maybe

parseInput :: String -> [[Char]]
parseInput = lines

countXmasInLine :: [Char] -> Int
countXmasInLine = length . filter (== "XMAS") . divvy 4 1

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : xs) !? i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

takeDiagonal :: [[Char]] -> Int -> [Char]
takeDiagonal mat = map (fromMaybe '.') . takeWhile isJust . zipWith (!?) mat . enumFrom

diagonals :: [[Char]] -> [[Char]]
diagonals mat =
  let n = length mat
   in reverse (takeDiagonal' mat [0 .. n - 1]) ++ takeDiagonal' (transpose mat) [1 .. n - 1]
  where
    takeDiagonal' mat' = map (takeDiagonal mat')

part1 :: [[Char]] -> Int
part1 board =
  let verticalFlip = transpose board
      southEast = diagonals board
      southWest = diagonals $ map reverse board
   in sum $ map ((+) <$> countXmas <*> countXmas') [board, verticalFlip, southEast, southWest]
  where
    countXmas = sum . map countXmasInLine
    countXmas' = sum . map (countXmasInLine . reverse)

part2 :: [[Char]] -> Int
part2 = length . concatMap (filter isXMas . divvy 3 1 . transpose) . divvy 3 1
  where
    isXMas c =
      let southEast = takeDiagonal c 0
          southWest = takeDiagonal (map reverse c) 0
       in isMas southWest && isMas southEast
      where
        isMas = (||) <$> (== "MAS") <*> (== "SAM")

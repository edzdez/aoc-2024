module Day07 where

import Data.List.Split

type Test = (Integer, [Integer])

parseInput :: String -> [Test]
parseInput =
  map
    ( \line ->
        let (a, b) = break (== ':') line
         in (read a, map read $ splitOn " " $ drop 2 b)
    )
    . lines

possiblyTrue :: Test -> Bool
possiblyTrue (res, ops) = possiblyTrue' 0 ops
  where
    possiblyTrue' acc [] = acc == res
    possiblyTrue' acc (x : xs) = (||) <$> possiblyTrue' (acc + x) <*> possiblyTrue' (acc * x) $ xs

possiblyTrue2 :: Test -> Bool
possiblyTrue2 (res, ops) = possiblyTrue' 0 0 ops
  where
    possiblyTrue' acc curr [] = acc + curr == res || acc * curr == res
    possiblyTrue' acc curr (x : xs)
      | curr == 0 = (acc <= res && possiblyTrue' acc x xs) || (concated <= res && possiblyTrue' concated 0 xs)
      | otherwise =
          (acc + curr <= res && possiblyTrue' (acc + curr) 0 (x : xs))
            || (acc * curr <= res && possiblyTrue' (acc * curr) 0 (x : xs))
            || (concated <= res && possiblyTrue' acc concated xs)
      where
        concated = read $ show acc ++ show x

part1 :: [Test] -> Integer
part1 = sum . map fst . filter possiblyTrue

part2 :: [Test] -> Integer
part2 = sum . map fst . filter possiblyTrue2

module Day03 (parseInput, part1, part2) where

import Text.Regex.TDFA

mulRegex :: String
mulRegex = "mul\\(([1-9][0-9]{0,2}),([1-9][0-9]{0,2})\\)"

doRegex :: String
doRegex = "do\\(\\)"

dontRegex :: String
dontRegex = "don't\\(\\)"

data Instruction = Mul [Int] | Do | Dont

isDo :: Instruction -> Bool
isDo Do = True
isDo _ = False

isDont :: Instruction -> Bool
isDont Dont = True
isDont _ = False

parseInput :: String -> [Instruction]
parseInput input = map parsePattern matches
  where
    fullPattern = mulRegex ++ "|" ++ doRegex ++ "|" ++ dontRegex
    matches = input =~ fullPattern :: [[String]]

    parsePattern (x : xs)
      | x == "do()" = Do
      | x == "don't()" = Dont
      | otherwise = Mul $ map read xs
    parsePattern _ = undefined

part1 :: [Instruction] -> Int
part1 = sum . map (product . toNums)
  where
    toNums (Mul xs) = xs
    toNums _ = [0]

part2 :: [Instruction] -> Int
part2 [] = 0
part2 (x : xs) = case x of
  Do -> part1 enabled + part2 disabled
  Dont -> part2 skipped
  _ -> part1 (x : enabled) + part2 disabled
  where
    enabled = takeWhile (not . isDont) xs
    disabled = dropWhile (not . isDont) xs
    skipped = dropWhile (not . isDo) xs

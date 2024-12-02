# AoC 2024

My solutions to the 2024 edition of Advent of Code (now in Haskell!).

## Build

This package is a cabal library, so you probably want to have cabal installed.

To run a particular solution (say day 1 part 1),

1. download your puzzle input into the `inputs/` folder (i.e. `inputs/01_input`),
2. start ghci through cabal via `cabal repl`,
3. import the correct module for the day (i.e. `import Day01`), and
4. run `test 01_input parseInput part1`

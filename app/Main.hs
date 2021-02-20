module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard
import GridShowers
import System.Exit
import Algs.NakedSubsets

isSolved = gridSolved classicBoard
solver  = refreshGridValues . updateUniqueValues classicBoard . updatePossibleValues classicBoard . ss2 . ss3
  where
    ss2 = nakedSubsetsN classicBoard 2
    ss3 = nakedSubsetsN classicBoard 3

harderGrid = readGridWith classicInit $ concat [
  "5...1.7..",
  "8........",
  "2....35..",
  "..1.9..6.",
  "3..851..7",
  ".8..4.3..",
  "..51....9",
  "........3",
  "..4.3...2"
  ]

main :: IO ()
main = do
  -- gridStr <- getLine
  let 
      -- grid = readGridWith classicInit gridStr
      grid = harderGrid
      updatedGrid = recursiveUpdateWith solver grid
      solved = isSolved updatedGrid
  putStrLn (showGridPV classicBoard grid)
  putStrLn (showGridPV classicBoard updatedGrid ++ "\n Solved: " ++ show solved)
  exitSuccess 


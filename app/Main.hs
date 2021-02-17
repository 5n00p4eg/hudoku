module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard
import GridShowers
import System.Exit

isSolved = gridSolved classicBoard
solver  = refreshGridValues . updateUniqueValues classicBoard . updatePossibleValues classicBoard

main :: IO ()
main = do
  gridStr <- getLine
  let grid = readGridWith classicInit gridStr
      updatedGrid = recursiveUpdateWith solver grid
      solved = isSolved updatedGrid
  putStrLn (showGridPV classicBoard grid)
  putStrLn (showGridPV classicBoard updatedGrid ++ "\n Solved: " ++ show solved)
  exitSuccess 


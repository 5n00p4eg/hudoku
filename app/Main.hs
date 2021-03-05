module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard
import GridShowers
import System.Exit
import Algs.NakedSubsets
import Algs.HiddenSets
import Checker

isSolved = gridSolved classicBoard
solver  = refreshGridValues . updateUniqueValues classicBoard . updatePossibleValues classicBoard . ss2 . ss3
  where
    ss2 = nakedSubsetsN classicBoard 2
    ss3 = nakedSubsetsN classicBoard 3
    hs2 = hiddenSubsetsN classicBoard 2

solver2  = refreshGridValues . updateUniqueValues classicBoard . updatePossibleValues classicBoard . ss2 . ss3 . hs2
  where
    ss2 = nakedSubsetsN classicBoard 2
    ss3 = nakedSubsetsN classicBoard 3
    hs2 = hiddenSubsetsN classicBoard 2

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

loggingSolver:: Grid -> IO Grid
loggingSolver g = do

  let solver = rf . uu . hs2 . ns2 . up
      up = updatePossibleValues classicBoard
      hs2 = hiddenSubsetsN classicBoard 2
      uu = updateUniqueValues classicBoard 
      rf = refreshGridValues
      ns2 = nakedSubsetsN classicBoard 2


  putStrLn $ "input grid: " ++ showGrid g
  putStrLn $ showGridPV classicBoard g
  let ret = solver g 
      isCorrect = isBoardCorrect classicBoard ret
  if not isCorrect then return $ error "Not correct" else return ret 

      
solveIO :: (Grid -> IO Grid) -> Grid -> IO Grid
solveIO s g = do
  update <- s g
  if update == g then return g else solveIO s update

main :: IO ()
main = do
  gridStr <- getLine
  let 
      grid = readGridWith classicInit gridStr
      -- grid = harderGrid
      updatedGrid = recursiveUpdateWith solver grid
      solved = isSolved updatedGrid

      updatedGrid2 = recursiveUpdateWith solver2 grid
      solved2 = isSolved updatedGrid2
--  print grid
  putStrLn $ showGrid grid
  putStrLn (showGridPV classicBoard grid)
  putStrLn (showGridPV classicBoard updatedGrid ++ "\n Solved: " ++ show solved)
  putStrLn (showGridPV classicBoard updatedGrid2 ++ "\n Solved: " ++ show solved2)

  putStrLn ("solver1: " ++ show solved ++ "\nSolver2: " ++ show solved2)

  ioGrid <- solveIO loggingSolver grid
  putStrLn $ showGrid ioGrid

  exitSuccess 



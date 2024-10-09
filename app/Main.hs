module Main where

import Algs.HiddenSets
import Board
import Checker
import ClassicBoard
import Data.Maybe
import Game
import Grid
import GridShowers
import System.Exit

isSolved = gridSolved classicBoard

up = updatePossibleValues classicBoard

hs2 = hiddenSubsetsN classicBoard 2

uu = updateUniqueValues classicBoard

rf = refreshGridValues

solver = rf . uu . up

solver2 = rf . uu . up . hs2

harderGrid =
  fromJust . readGrid $
    concat
      [ "5...1.7..",
        "8........",
        "2....35..",
        "..1.9..6.",
        "3..851..7",
        ".8..4.3..",
        "..51....9",
        "........3",
        "..4.3...2"
      ]

loggingSolver :: Grid -> IO Grid
loggingSolver g = do
  let solver = rf . uu . up . hs2

  putStrLn $ "input grid: " ++ showGrid g
  putStrLn $ showGridPV classicBoard g
  let ret = solver g
      isCorrect = isBoardCorrect classicBoard ret
  if not isCorrect then return $ error "Not correct" else return ret

solveIO :: (Grid -> IO Grid) -> Grid -> IO Grid
solveIO s g = do
  update <- s g
  if update == g then return g else solveIO s update

monadSolver :: Game Bool
monadSolver = do
  initPossibleValues

  grid <- getGrid
  let solved = recursiveUpdateWith solver grid
  setGrid solved
  gameSolved

main :: IO ()
main = do
  -- gridStr <- getLine
  let -- grid = readGridWith classicInit gridStr
      grid = harderGrid
  -- updatedGrid = recursiveUpdateWith solver grid
  -- solved = isSolved updatedGrid

  -- updatedGrid2 = recursiveUpdateWith solver2 grid
  -- solved2 = isSolved updatedGrid2
  -- print grid
  -- putStrLn $ showGrid grid
  -- putStrLn (showGridPV classicBoard grid)
  -- putStrLn (showGridPV classicBoard updatedGrid ++ "\n Solved: " ++ show solved)
  -- putStrLn (showGridPV classicBoard updatedGrid2 ++ "\n Solved: " ++ show solved2)

  -- putStrLn ("solver1: " ++ show solved ++ "\nSolver2: " ++ show solved2)

  -- ioGrid <- solveIO loggingSolver grid
  -- putStrLn $ showGrid ioGrid
  let -- game = setupGame harderGrid
      -- grid = getGrid game
      (solved, grid) = runGame classicBoard harderGrid monadSolver
  print grid
  print solved

  exitSuccess

module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard
import GridShowers

-- test_grid = fromJust $ readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
--test_grid = fromJust $ readGrid "5...1..7..8........2....35..751392.6.346851..7982.4.3..63512...9.28.....3.74.3...2"
test_grid = fromJust $ readGrid $ concat [
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

-- test_grid = fromJust $ readGrid "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
-- test_grid = fromJust $ readGrid "1234567899.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

init_grid = initPossibleValues classic_board test_grid

up_grid   = updatePossibleValues classic_board init_grid

nthGrid :: Board -> Grid -> Int -> Grid
nthGrid _ g 0 = g
nthGrid b g n = nthGrid b (refreshGridValues $ updatePossibleValues b g) (n - 1)

-- solved_grid = nthGrid classic_board up_grid 11

-- is_solved = gridSolved classic_board solved_grid

solved_grid = recursiveUpdate classic_board (initPossibleValues classic_board test_grid)
is_solved = gridSolved classic_board solved_grid

main :: IO ()
main = do
  putStrLn (showGridPV classic_board test_grid)
  putStrLn (showGridPV classic_board solved_grid ++ "\n Solved: " ++ show is_solved)

-- TEsting lines:
-- Just grid = readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
-- showGrid grid

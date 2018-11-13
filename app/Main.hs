module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard
import GridShowers

-- test_grid = fromJust $ readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
test_grid = fromJust $ readGrid "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
-- test_grid = fromJust $ readGrid "1234567899.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

init_grid = initPossibleValues classic_board test_grid

up_grid   = updatePossibleValues classic_board init_grid

nthGrid :: Board -> Grid -> Int -> Grid
nthGrid _ g 0 = g
nthGrid b g n = nthGrid b (refreshGridValues $ updatePossibleValues b g) (n - 1)

main :: IO ()
main = putStrLn (showGridPV classic_board $ nthGrid classic_board up_grid 13)

-- TEsting lines:
-- Just grid = readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
-- showGrid grid

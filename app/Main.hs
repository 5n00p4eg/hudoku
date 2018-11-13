module Main where

import Grid
import Board
import Data.Maybe
import ClassicBoard

-- test_grid = fromJust $ readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
test_grid = fromJust $ readGrid "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
-- test_grid = fromJust $ readGrid "1234567899.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

init_grid = initPossibleValues classic_board test_grid

up_grid   = updatePossibleValues classic_board init_grid

refreshed_gird   = refreshGridValues up_grid

nthGrid :: Board -> Grid -> Int -> Grid
nthGrid _ g 0 = g
nthGrid b g n = nthGrid b (refreshGridValues $ updatePossibleValues b g) (n - 1)

refreshed_gird2  = updatePossibleValues classic_board refreshed_gird
refreshed_gird3  = updatePossibleValues classic_board refreshed_gird2
refreshed_gird4  = updatePossibleValues classic_board refreshed_gird3
refreshed_gird5  = updatePossibleValues classic_board refreshed_gird4
refreshed_gird6  = updatePossibleValues classic_board refreshed_gird5
refreshed_gird7  = updatePossibleValues classic_board refreshed_gird6
refreshed_gird8  = updatePossibleValues classic_board refreshed_gird7
refreshed_gird9  = updatePossibleValues classic_board refreshed_gird8
refreshed_gird10 = updatePossibleValues classic_board refreshed_gird9

main :: IO ()
main = print (show classic_board)

-- TEsting lines:
-- Just grid = readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
-- showGrid grid

module Main where

import Grid
import Data.Maybe
import ClassicBoard

test_grid = fromJust $ readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

main :: IO ()
main = print (show classic_board)

-- TEsting lines:
-- Just grid = readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
-- showGrid grid

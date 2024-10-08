module GridShowers where

import Board
import Grid
import Text.PrettyPrint.Boxes
import Prelude hiding ((<>))

-- vDiv :: Int -> B.Box
-- vDiv n = vcat left (replicate n (char '|'))

-- hDiv :: Int -> B.Box
-- hDiv n = hcat left (replicate n (char '-'))

showGridPV :: Board -> Grid -> String
showGridPV board grid = render $ foldl (//) nullBox rows
  where
    rows = map row [1 .. (boardSize board !! 1)]
    row y = foldl (<>) nullBox (cells y)
    cells y = map (cell y) [1 .. head (boardSize board)]
    cell x y = text $ showCellData (cell' x y)
    cell' x y = posToCell board grid (Position [x, y])

-- map () 1..x -- each row
-- map () 1..y --

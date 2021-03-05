module Checker where
import Board
import Grid
  -- import Data.Maybe
import Data.List

-- TODO: Cover
isBoardCorrect :: Board -> Grid -> Bool
isBoardCorrect b g = all groupCorrect $ boardGroups b
  where
    groupCorrect :: Group -> Bool
    groupCorrect g = foldl (\c (val, freq) -> c && freq == 1) True $ groupFreq g

    groupFreq :: Group -> [(Int, Int)]
    groupFreq g = map(\x -> (head x, length x)) . group . sort . map cellValue $ values g
    values :: Group -> [Cell]
    values g = filter isCellValue $ map cell g
    cell :: Position -> Cell
    cell p = posToCell b g p







module Grid where

import Data.Char
import Data.Maybe
import Prelude

data Cell = CellValue Int | EmptyCellVallue | PossibleValues [Int] deriving (Eq)

instance Show Cell where
  show (CellValue a) = show a
  show EmptyCellVallue = "."
  show (PossibleValues x) = show x

isCellValue (CellValue _) = True
isCellValue _ = False

cellValue :: Cell -> Int
cellValue (CellValue n) = n
cellValue _ = error "Cell is not value"

isEmptyCell EmptyCellVallue = True
isEmptyCell _ = False

isPossibleValues (PossibleValues _) = True
isPossibleValues _ = False

cellCandidates :: Cell -> [Int]
cellCandidates (PossibleValues x) = x
cellCandidates _ = error "Cell can't have candidates"

isPossibleValuesHasValue :: Cell -> Int -> Bool
isPossibleValuesHasValue (PossibleValues vals) x = x `elem` vals
isPossibleValuesHasValue _ _ = False

type Grid = [Cell]

{- TODO: Rework, print as table-}
showGrid :: Grid -> String
showGrid = concatMap show

showCellData :: Cell -> String
showCellData (CellValue n) = show n
showCellData EmptyCellVallue = "."
showCellData (PossibleValues p) = show p

refreshGridValues :: Grid -> Grid
refreshGridValues = map refreshGridValue

refreshGridValue :: Cell -> Cell
refreshGridValue (PossibleValues p) = if length p == 1 then CellValue (head p) else PossibleValues p
refreshGridValue x = x

readGrid :: String -> Maybe Grid
readGrid = traverse readCell
  where
    readCell '.' = Just EmptyCellVallue
    readCell '0' = Just EmptyCellVallue
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . CellValue . Data.Char.digitToInt $ c
      | otherwise = Nothing

readGridWith :: (Grid -> Grid) -> String -> Grid
readGridWith f = f . fromJust . readGrid

removeCellCandidates :: Cell -> [Int] -> Cell
removeCellCandidates (PossibleValues p) list = PossibleValues (filter (`notElem` list) p)
removeCellCandidates x _ = x

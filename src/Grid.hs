module Grid where

import Prelude
import Data.Char

import Data.Maybe

data Cell = CellValue Int | EmptyCellVallue | PossibleValues [Int] deriving (Eq)
instance Show Cell where
  show (CellValue a) = show a
  show (EmptyCellVallue) = "."
  show (PossibleValues _) = "_"

isCellValue (CellValue _) = True
isCellValue _             = False

isEmptyCell EmptyCellVallue = True 
isEmptyCell _               = False 

isPossibleValues (PossibleValues _) = True 
isPossibleValues _                  = False 

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
refreshGridValue (PossibleValues p) = if length p == 1 then CellValue (head p) else (PossibleValues p)
refreshGridValue x = x

--  TODO: Вввод-вывод доски
--  TODO: Синонимы на общие виды досок (2d 9x9, 3d 9x9, 2d 16*16)

readGrid :: String -> Maybe Grid
readGrid s = traverse readCell s
  where
    readCell '.' = Just $ EmptyCellVallue
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . CellValue . Data.Char.digitToInt $ c
      | otherwise = Nothing

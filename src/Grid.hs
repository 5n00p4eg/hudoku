module Grid where

import Prelude
import Data.Char

-- We can process N-dimensional sudoku.
data Position = Position [Int] deriving (Show, Eq);
data Cell = CellValue Int | EmptyCellVallue | PossibleValues [Int] deriving (Eq)
instance Show Cell where
  show (CellValue a) = show a
  show (EmptyCellVallue) = "."
  show (PossibleValues _) = "_"

type Grid = [Cell]

showGrid :: Grid -> String
showGrid g = concat $ map show g

type Group = [Position]

-- Dim, size, groups
data Board = Board Int Int [Group]
instance Show Board where
  show (Board d s g) = "Dim: " ++ show d ++ ", Size: " ++ show s ++ ", Groups: " ++ show (length g)

getPossibleValues :: Cell -> [Int]
getPossibleValues (PossibleValues v) = v
getPossibleValues _ = []

showGroups :: [Group]  -> String
showGroups [] = ""
showGroups g = show (head g)  ++ "\n" ++ (showGroups (tail g))

showBoardGroups :: Board -> String
showBoardGroups (Board _ _ g) = showGroups g

--  TODO: Вввод-вывод доски
--  TODO: Синонимы на общие виды досок (2d 9x9, 3d 9x9, 2d 16*16)

readGrid :: String -> Maybe Grid
readGrid s = traverse readCell s
  where
    readCell '.' = Just $ EmptyCellVallue
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . CellValue . Data.Char.digitToInt $ c
      | otherwise = Nothing

cellGetGroups :: Board -> Position -> [Group]
cellGetGroups (Board _ _ gs) p = filter (elem p) gs

initPossibleValues :: Board -> Grid -> Grid
initPossibleValues (Board _ s _) grs = map (initCell) grs
  where
    initCell EmptyCellVallue = PossibleValues [1..s]
    initCell a = a

updatePossibleValues :: Board -> Grid -> Grid
updatePossibleValues (Board _ s gs) grs = map (initCell) grs

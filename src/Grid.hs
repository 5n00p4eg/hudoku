module Grid where

import Prelude
import Data.Char
import Data.List
import Data.Maybe

-- We can process N-dimensional sudoku.
data Position = Position [Int] deriving (Show, Eq);

type PositionList = [(Int, Position)]
data Cell = CellValue Int | EmptyCellVallue | PossibleValues [Int] deriving (Eq)
instance Show Cell where
  show (CellValue a) = show a
  show (EmptyCellVallue) = "."
  show (PossibleValues _) = "_"

type Grid = [Cell]

{- Rework, print as table-}
showGrid :: Grid -> String
showGrid g = concat $ map show g

type Group = [Position]

-- Dim, size, groups
data Board = Board Int Int [Group] PositionList
instance Show Board where
  show (Board d s g _ ) = "Dim: " ++ show d ++ ", Size: " ++ show s ++ ", Groups: " ++ show (length g)

getPossibleValues :: Cell -> [Int]
getPossibleValues (PossibleValues v) = v
getPossibleValues _ = []

showGroups :: [Group]  -> String
showGroups [] = ""
showGroups g = show (head g)  ++ "\n" ++ (showGroups (tail g))

showBoardGroups :: Board -> String
showBoardGroups (Board _ _ g _) = showGroups g

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
cellGetGroups (Board _ _ gs _) p = filter (elem p) gs

initPossibleValues :: Board -> Grid -> Grid
initPossibleValues (Board _ s _ _) grs = map initCell grs
  where
    initCell EmptyCellVallue = PossibleValues [1..s]
    initCell a = a

posToCell:: Board -> Grid -> Position -> Cell
posToCell b g p = g !! (posToNum b g p)

posToNum:: Board -> Grid -> Position -> Int
posToNum (Board _ _ _ pl) g p = pos p
  where
    pos x = fst (fromJust $ pos' x) - 1
    pos' x = find (\ (_, pos)-> pos == x) pl

{-
updatePossibleValues :: Board -> Grid -> Grid
updatePossibleValues (Board _ s gs pl) grs = map updateGroup gs
  where
    updateGroup gr = map () gr
-}

{-
For each group
get all CellValue
remove it's all from all possible values.

====


 -}

module Board where

import Grid
import Data.List
import Data.Maybe
-- We can process N-dimensional sudoku.
data Position = Position [Int] deriving (Show, Eq);

type PositionList = [(Int, Position)]

type Group = [Position]

-- Dim, size, groups
data Board = Board Int Int [Group] PositionList
instance Show Board where
  show (Board d s g _ ) = "Dim: " ++ show d ++ ", Size: " ++ show s ++ ", Groups: " ++ show (length g)

showBoardGroups :: Board -> String
showBoardGroups (Board _ _ g _) = showGroups g

getPossibleValues :: Cell -> [Int]
getPossibleValues (PossibleValues v) = v
getPossibleValues _ = []

showGroups :: [Group]  -> String
showGroups [] = ""
showGroups g = show (head g)  ++ "\n" ++ (showGroups (tail g))


cellGetGroups :: Board -> Position -> [Group]
cellGetGroups (Board _ _ gs _) p = filter (elem p) gs

initPossibleValues :: Board -> Grid -> Grid
initPossibleValues (Board _ s _ _) grs = map initCell grs
  where
    initCell EmptyCellVallue = PossibleValues [1..s]
    initCell a = a

posToCell:: Board -> Grid -> Position -> Cell
posToCell b g p = g !! (posToNum b p)

posToNum:: Board -> Position -> Int
posToNum (Board _ _ _ pl) p = pos p
  where
    pos x = fst (fromJust $ pos' x) - 1
    pos' x = find (\ (_, pos)-> pos == x) pl

numToPos:: Board -> Grid -> Int -> Position
numToPos (Board _ _ _ pl) g p = snd (fromJust $ find (\ (i, _) -> i==p) pl)


updatePossibleValues :: Board -> Grid -> Grid
updatePossibleValues (Board d s gs pl) grid = map (updateCell) gridToMap
  where
    updateCell (_, CellValue n) = CellValue n
    updateCell (pos, (PossibleValues pvals)) = PossibleValues (filterPvals pos pvals)
    -- updateCell (pos, (PossibleValues pvals)) = PossibleValues (getActiveVals (groupsByPos pos))
    filterPvals pos pvals = filter (\old -> notElem old (getActiveVals (groupsByPos pos))) pvals
    gridToMap = map (\n -> (snd n, grid !! ((fst n) - 1))) pl
    groupsByPos pos = filter (elem pos) gs
    getActiveVals grps = nub $ map (fromJust . getActiveVals'') $ filter (filterAval) $ getActiveVals' grps
    --  Get array of Cell's
    getActiveVals' grps = map (posToCell') (nub $ concat grps)
    getActiveVals'' (CellValue a) = Just a
    getActiveVals'' _ = Nothing
    filterAval (CellValue _) = True
    filterAval _ = False
    posToCell' = posToCell (Board d s gs pl) grid

boardSize :: Board -> [Int]
boardSize (Board d s g pl) = map dimSize [1..d]
  where
    dimSize d = maximum $ dimSize' d
    dimSize' d = map (\ (_, Position pos) -> pos !! (d - 1)) pl

boardDimensions :: Board -> Int
boardDimensions (Board d _ _ _) = d

groupSolved :: Board -> Group -> Grid -> Bool
groupSolved board group grid = not (hasNothing group || notEqual group)
  where
    hasNothing g = or (map (isNothing . cellToValue . posToCell') g)
    notEqual g = targetValues /= sort (map (fromJust . cellToValue . posToCell') g)
    posToCell' = posToCell board grid
    cellToValue (CellValue a) = Just a
    cellToValue _ = Nothing
    targetValues = [1..(length group)]

gridSolved :: Board -> Grid -> Bool
gridSolved (Board d s gs pl) grid = and $ map (\g -> groupSolved (Board d s gs pl) g grid) gs

recursiveUpdate :: Board -> Grid -> Grid
recursiveUpdate b grid
  | grid == update = grid
  | True = recursiveUpdate b update
  where
    update = refreshGridValues $ updatePossibleValues b grid

module Algs.NakedSubsets where

import Board
import Grid
import Control.Applicative
import Data.Maybe
import Data.List

{-|
  This is generic algorithm for naken pairs, triples, etc.
  
  The idea is:
  1. get all the board groups and update grid after each group handling.
  2. For each group we're running same generalized algorighm parametrized with N (2 for pairs).
  3. Algorithm look for set of N cells that conatin only n<=N same candidates.
  4. Then we can remove all N candidates from rest of grouop.

-}
nakedSubsetsN :: Board -> Int -> Grid -> Grid
nakedSubsetsN board size grid = foldl grp grid [0..length (boardGroups board) - 1]
  where
    grp curry gn = nakedSubsetsNGroupN board size gn curry

nakedSubsetsNGroupN :: Board -> Int -> Int -> Grid -> Grid
nakedSubsetsNGroupN board n groupNumber grid = applyRestult
  where
    group = boardGroups board !! groupNumber
    cells :: [CellInfo]
    cells = groupToCellInfo board grid group
    handleGroup' = handleGroup n
    results :: [CellInfo]
    results = handleGroup' cells
    applyRestult = updateGridWithValues board grid (map (\ci -> (cellInfoPosition ci, cellInfoCell ci)) results)

handleGroup :: Int -> [CellInfo] -> [CellInfo]
handleGroup n group = foldl handleGroup' group group
  where
    handleGroup' :: [CellInfo] -> CellInfo -> [CellInfo]
    handleGroup' = handleCell n 

handleCell :: Int -> [CellInfo] -> CellInfo -> [CellInfo]
handleCell _ g (_, _, CellValue _) = g
handleCell n group cell = if hasNakedN then result else group
  where
    groupNoCell :: [CellInfo]
    groupNoCell = filter (\ci -> cellInfoIndex ci /= cellInfoIndex cell) group
    filterPVLength :: CellInfo -> Bool 
    filterPVLength (_, _, PossibleValues x) = length x <= n
    filterPVLength _ = False 
    filterNaked :: CellInfo -> Bool 
    filterNaked (_, _, CellValue _ ) = False 
    filterNaked x = null $ cellCandidates (removeCellCandidates (cellInfoCell x) $ cellCandidates $ cellInfoCell cell)

    hasNakedN :: Bool
    hasNakedN = length (filter filterNaked group) == n && filterPVLength cell
    groupToUpdate :: [CellInfo]
    groupToUpdate = filter (not . filterNaked) groupNoCell
--    groupToUpdate = filter (not . groupToUpdate') groupNoCell
--    groupToUpdate' = (&&) <$> filterNaked <*> filterPVLength

    updatedCells :: [CellInfo]
    updatedCells = map (\ci -> (cellInfoPosition ci, cellInfoIndex ci, removeCellCandidates (cellInfoCell ci) candidatesToRemove )) groupToUpdate

    candidatesToRemove :: [Int]
    candidatesToRemove = cellCandidates $ cellInfoCell cell
    
    result :: [CellInfo]
    result = map result' group
    result' :: CellInfo -> CellInfo 
    result' o = fromMaybe o (result'' o)
    result'' :: CellInfo -> Maybe CellInfo
    result'' x = find (\ci -> cellInfoIndex ci == cellInfoIndex x) updatedCells

    resultA = []


module Algs.NakedSubsetsMonadic where

import Board
import Control.Applicative
import Data.List
import Data.Maybe
import Game
import Grid

-- |
--  This is generic algorithm for naked pairs, triples, etc.
--
--  The idea is:
--  1. get all the board groups and update grid after each group handling.
--  2. For each group we're running same generalized algorighm parametrized with N (2 for pairs).
--  3. Algorithm look for set of N cells that conatin only n<=N same candidates.
--  4. Then we can remove all N candidates from rest of grouop.
--
--  5. Monadic means it return a list of updates
type SubsetSize = Int

findNakedSubsetsN :: SubsetSize -> Game [(GroupIndex, [CellInfo])]
findNakedSubsetsN size = do
  board <- getBoard
  return []

-- findNakedSubsetsNGroupN :: SubsetSize -> GroupIndex -> Game [(GroupIndex, [CellInfo])]
-- findNakedSubsetsNGroupN size groupIndex = do
-- board <- getBoard
-- let
-- group = boardGroups board !! groupIndex

-- | This function is the low-level algorighm implementation
-- It returns updated list of cells, diff can be extraceted later.
-- TODO: extract subsets
findNakedSubsetsNCells :: SubsetSize -> [Cell] -> [Cell]
findNakedSubsetsNCells size input = result
  where
    possibleSubsets :: [[Int]]
    possibleSubsets = filter (\l -> length l <= size) $ subsequences possibleSubsets'
    possibleSubsets' = dedup $ concatMap getPossibleValues input

    dedup = map head . group . sort

    checkSubset :: [Int] -> Bool
    checkSubset subset = checkSubsetCount subset == size

    checkSubsetCount :: [Int] -> Int
    checkSubsetCount subset = length $ filter id $ checkSubsetPVCells subset

    checkSubsetPVCells :: [Int] -> [Bool]
    checkSubsetPVCells subset = fmap (checkSubsetPVCells' subset) input

    -- It checks if cell has only values from subset
    checkSubsetPVCells' :: [Int] -> Cell -> Bool
    checkSubsetPVCells' subset (PossibleValues pv) = null $ pv \\ subset
    checkSubsetPVCells' subset _ = False

    applySubset :: [Int] -> Cell -> Cell
    applySubset subset cell =
      if checkSubsetPVCells' subset cell
        then cell
        else removeCellCandidates cell subset

    applySubsets :: [[Int]] -> Cell -> Cell
    applySubsets subsets cell = foldl (flip applySubset) cell subsets

    subsets = filter checkSubset possibleSubsets

    result = fmap (applySubsets subsets) input

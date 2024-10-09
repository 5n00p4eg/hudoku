module Algs.NakedSubsetsMonadicTest (tests) where

import Algs.NakedSubsetsMonadic
import Board (boardGridLength)
import ClassicBoard (classicBoard, classicGame)
import Data.IntSet (fromList)
import Game (evalGame, initGame)
import Grid
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "NakedSubsets (Monadic) algorithm tests" [findNakedSubsetsNCellsTestGroup, findNakedSubsetsNGroupNTestGroup]

findNakedSubsetsNCellsTestGroup = testGroup "findNakedSubsetsNCells" [cellsSize2TestGroup, cellsSize3TestGroup, cellsSize4TestGroup]

cellsSize2TestGroup = testGroup "cells only size 2 tests" [cellSize2BasicTestCase]

cellsSize3TestGroup = testGroup "cells only size 3 tests" [cellSize3BasicTestCase]

cellsSize4TestGroup = testGroup "cells only size 4 tests" [cellSize4BasicTestCase]

cellSize2BasicTestCase = testCase "cellSize2BasicTestCase" $ assertEqual "" expected eval
  where
    expected =
      [ CellValue 7,
        CellValue 6,
        PossibleValues $ fromList [4, 8],
        CellValue 9,
        CellValue 1,
        PossibleValues $ fromList [4, 8],
        PossibleValues $ fromList [2, 3],
        CellValue 5,
        PossibleValues $ fromList [2, 3]
      ]
    input =
      [ CellValue 7,
        CellValue 6,
        PossibleValues $ fromList [2, 3, 4, 8],
        CellValue 9,
        CellValue 1,
        PossibleValues $ fromList [3, 4, 8],
        PossibleValues $ fromList [2, 3],
        CellValue 5,
        PossibleValues $ fromList [2, 3]
      ]
    eval = findNakedSubsetsNCells 2 input

cellSize3BasicTestCase = testCase "cellSize3BasicTestCase" $ assertEqual "" expected eval
  where
    expected =
      [ PossibleValues $ fromList [7, 8, 9],
        CellValue 1,
        PossibleValues $ fromList [7, 8],
        PossibleValues $ fromList [3, 5],
        CellValue 4,
        PossibleValues $ fromList [5, 6],
        PossibleValues $ fromList [7, 9],
        CellValue 2,
        PossibleValues $ fromList [3, 5, 6]
      ]
    input =
      [ PossibleValues $ fromList [7, 8, 9],
        CellValue 1,
        PossibleValues $ fromList [7, 8],
        PossibleValues $ fromList [3, 5, 9],
        CellValue 4,
        PossibleValues $ fromList [5, 6, 8, 9],
        PossibleValues $ fromList [7, 9],
        CellValue 2,
        PossibleValues $ fromList [3, 5, 6, 7, 8, 9]
      ]
    eval = findNakedSubsetsNCells 3 input

cellSize4BasicTestCase = testCase "cellSize4BasicTestCase" $ assertEqual "" expected eval
  where
    expected =
      [ CellValue 1,
        PossibleValues $ fromList [5, 6],
        PossibleValues $ fromList [4, 9],
        PossibleValues $ fromList [3, 5, 6],
        PossibleValues $ fromList [3, 5, 6, 7],
        PossibleValues $ fromList [3, 5, 7],
        PossibleValues $ fromList [2, 4, 8, 9],
        PossibleValues $ fromList [2, 4],
        PossibleValues $ fromList [2, 8, 9]
      ]
    input =
      [ CellValue 1,
        PossibleValues $ fromList [4, 5, 6],
        PossibleValues $ fromList [4, 9],
        PossibleValues $ fromList [3, 5, 6],
        PossibleValues $ fromList [3, 5, 6, 7],
        PossibleValues $ fromList [3, 5, 7],
        PossibleValues $ fromList [2, 4, 8, 9],
        PossibleValues $ fromList [2, 4],
        PossibleValues $ fromList [2, 8, 9]
      ]
    eval = findNakedSubsetsNCells 4 input

findNakedSubsetsNGroupNTestGroup = testGroup "findNakedSubsetsNGroupN " [subsetsSize2TestGroup, subsetsSize3TestGroup, subsetsSize4TestGroup]

subsetsSize2TestGroup = testGroup "cells only size 2 tests" [subsetsSize2BasicTestCase, subsetsSize2TwoPairsTestCase]

subsetsSize3TestGroup = testGroup "cells only size 3 tests" [subsetsSize3BasicTestCase]

subsetsSize4TestGroup = testGroup "cells only size 4 tests" [subsetsSize4BasicTestCase]

findNakedSubsetsNGroupNTestGame = classicGame

findNakedSubsetsNGroupNTestEval :: SubsetSize -> [Cell] -> [Subset]
findNakedSubsetsNGroupNTestEval size input = evalGame classicBoard grid $ findNakedSubsetsNGroupN size 0
  where
    grid :: Grid
    grid = extendList len EmptyCellVallue input

    extendList :: Int -> Cell -> [Cell] -> Grid
    extendList n defaultVal xs = take n (xs ++ replicate (n - length xs) defaultVal)

    len = boardGridLength classicBoard

subsetsSize2BasicTestCase = testCase "cellSize2BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [fromList [2, 3]]
    input =
      [ CellValue 7,
        CellValue 6,
        PossibleValues $ fromList [2, 3, 4, 8],
        CellValue 9,
        CellValue 1,
        PossibleValues $ fromList [3, 4, 8],
        PossibleValues $ fromList [2, 3],
        CellValue 5,
        PossibleValues $ fromList [2, 3]
      ]
    eval = findNakedSubsetsNGroupNTestEval 2 input

subsetsSize2TwoPairsTestCase = testCase "cellSize2BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [fromList [2, 3], fromList [4, 5]]
    input =
      [ CellValue 7,
        CellValue 6,
        PossibleValues $ fromList [2, 3, 4, 8],
        PossibleValues $ fromList [4, 5],
        CellValue 1,
        PossibleValues $ fromList [4, 5],
        PossibleValues $ fromList [2, 3],
        CellValue 5,
        PossibleValues $ fromList [2, 3]
      ]
    eval = findNakedSubsetsNGroupNTestEval 2 input

subsetsSize3BasicTestCase = testCase "cellSize3BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [fromList [7, 8, 9]]
    input =
      [ PossibleValues $ fromList [7, 8, 9],
        CellValue 1,
        PossibleValues $ fromList [7, 8],
        PossibleValues $ fromList [3, 5, 9],
        CellValue 4,
        PossibleValues $ fromList [5, 6, 8, 9],
        PossibleValues $ fromList [7, 9],
        CellValue 2,
        PossibleValues $ fromList [3, 5, 6, 7, 8, 9]
      ]
    eval = findNakedSubsetsNGroupNTestEval 3 input

subsetsSize4BasicTestCase = testCase "subsetSize4BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [fromList [2, 4, 8, 9]]
    input =
      [ CellValue 1,
        PossibleValues $ fromList [4, 5, 6],
        PossibleValues $ fromList [4, 9],
        PossibleValues $ fromList [3, 5, 6],
        PossibleValues $ fromList [3, 5, 6, 7],
        PossibleValues $ fromList [3, 5, 7],
        PossibleValues $ fromList [2, 4, 8, 9],
        PossibleValues $ fromList [2, 4],
        PossibleValues $ fromList [2, 8, 9]
      ]
    eval = findNakedSubsetsNGroupNTestEval 4 input

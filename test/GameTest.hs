module GameTest (tests) where

import Data.IntSet (fromList)
import Game
import Grid
import Test.Tasty
import Test.Tasty.HUnit
import TestBoard1d

tests =
  testGroup
    "Game module tests"
    [ gameSolvedTestGroup,
      initPossibleValuesTestGroup
    ]

gameSolvedTestGroup =
  testGroup
    "gameSolved function tests"
    [ simplestPositiveCase,
      simplestNegativeCase
    ]

simplestPositiveCase = testCase "Check solved simplest grid" $ assertEqual "grid is solved" True eval
  where
    eval = evalGame testBoard1d grid gameSolved
    grid = [CellValue 1, CellValue 2, CellValue 3, CellValue 4, CellValue 5]

simplestNegativeCase = testCase "Check not solved simplest grid" $ assertEqual "grid is not solved" False eval
  where
    eval = evalGame testBoard1d grid gameSolved
    grid = [CellValue 1, CellValue 2, CellValue 3, CellValue 4, PossibleValues $ fromList [5]]

initPossibleValuesTestGroup =
  testGroup
    "initPossibleValues function tests"
    [ emptyGridTestCase
    ]

emptyGridTestCase = testCase "Check for empty input" $ assertEqual "all PV" expected eval
  where
    expected = replicate 5 (PossibleValues $ fromList [1, 2, 3, 4, 5])
    eval = execGame testBoard1d initial initPossibleValues
    initial = replicate 5 EmptyCellVallue

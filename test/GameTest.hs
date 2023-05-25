module GameTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Grid
import TestBoard1d
import Game

tests = testGroup "Game module tests" [
    gameSolvedTestGroup,
    initPossibleValuesTestGroup
                                      ]

gameSolvedTestGroup = testGroup "gameSolved function tests" [
        simplestPositiveCase,
        simplestNegativeCase
                                                            ]

simplestPositiveCase = testCase "Check solved simplest grid" $ assertEqual "grid is solved" True eval
    where
        eval = evalGame testBoard1d grid gameSolved
        grid = [CellValue 1, CellValue 2, CellValue 3, CellValue 4, CellValue 5]

simplestNegativeCase = testCase "Check not solved simplest grid" $ assertEqual "grid is not solved" False eval
    where
        eval = evalGame testBoard1d grid gameSolved
        grid = [CellValue 1, CellValue 2, CellValue 3, CellValue 4, PossibleValues [5]]


initPossibleValuesTestGroup = testGroup "initPossibleValues function tests" [
    emptyGridTestCase
                                                                            ]
                                                                    
emptyGridTestCase = testCase "Check for empty input" $ assertEqual "all PV" expected eval
    where
        expected = take 5 (repeat $ PossibleValues [1,2,3,4,5])
        eval = execGame testBoard1d initial (initPossibleValues )
        initial = take 5 (repeat $ EmptyCellVallue)
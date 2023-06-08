module Algs.NakedSubsetsMonadicTest (tests) where


import Test.Tasty
import Test.Tasty.HUnit

import Grid

import Algs.NakedSubsetsMonadic

tests = testGroup "NakedSubsets (Monadic) algorithm tests"  [ cellsSize2TestGroup, cellsSize3TestGroup, cellsSize4TestGroup ] 

cellsSize2TestGroup = testGroup "cells only size 2 tests" [ cellSize2BasicTestCase ]
cellsSize3TestGroup = testGroup "cells only size 3 tests" [ cellSize3BasicTestCase ]
cellsSize4TestGroup = testGroup "cells only size 4 tests" [ cellSize4BasicTestCase ]

cellSize2BasicTestCase = testCase "cellSize2BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [
        CellValue 7,
        CellValue 6,
        PossibleValues [4,8],
        CellValue 9,
        CellValue 1,
        PossibleValues [4,8],
        PossibleValues [2,3],
        CellValue 5,
        PossibleValues [2,3]
        ]
    input = [
        CellValue 7,
        CellValue 6,
        PossibleValues [2, 3, 4, 8],
        CellValue 9,
        CellValue 1,
        PossibleValues [3,4,8],
        PossibleValues [2,3],
        CellValue 5,
        PossibleValues [2,3]
        ]
    eval = findNakedSubsetsNCells 2 input

cellSize3BasicTestCase = testCase "cellSize3BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [
        PossibleValues [7,8,9],
        CellValue 1,
        PossibleValues [7,8],
        PossibleValues [3,5],
        CellValue 4,
        PossibleValues [5,6],
        PossibleValues [7,9],
        CellValue 2,
        PossibleValues [3,5,6]
        ]
    input = [
        PossibleValues [7,8,9],
        CellValue 1,
        PossibleValues [7,8],
        PossibleValues [3,5,9],
        CellValue 4,
        PossibleValues [5,6,8,9],
        PossibleValues [7,9],
        CellValue 2,
        PossibleValues [3,5,6,7,8,9]
        ]
    eval = findNakedSubsetsNCells 3 input

cellSize4BasicTestCase = testCase "cellSize4BasicTestCase" $ assertEqual "" expected eval
  where
    expected = [
        CellValue 1,
        PossibleValues [5,6],
        PossibleValues [4,9],
        PossibleValues [3,5,6],
        PossibleValues [3,5,6,7],
        PossibleValues [3,5,7],
        PossibleValues [2,4,8,9],
        PossibleValues [2,4],
        PossibleValues [2,8,9]
        ]
    input = [
        CellValue 1,
        PossibleValues [4,5,6],
        PossibleValues [4,9],
        PossibleValues [3,5,6],
        PossibleValues [3,5,6,7],
        PossibleValues [3,5,7],
        PossibleValues [2,4,8,9],
        PossibleValues [2,4],
        PossibleValues [2,8,9]
        ]
    eval = findNakedSubsetsNCells 4 input


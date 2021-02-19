module Algs.NakedSubsetsTest (tests) where

import Board
import Grid
import Test.Tasty
import Test.Tasty.HUnit
import TestBoard1d
import Algs.NakedSubsets

tests = testGroup "NakedSubsets algorithm tests"  [ nakedSubsets2TestGroup, handleCellTestGroup, handleGroupTestCase ] 

nakedSubsets2TestGroup = testGroup "nakedSubsets2 tests" [ nakedSubsets2TestCase ]

nakedSubsets2TestCaseBoard = updateGridWithValues testBoard1d source vals
  where
    source = readGridWith (updatePossibleValues testBoard1d .  testBoardInit ) "...45"
    vals = [
      (Position [1], PossibleValues [1,2]),
      (Position [2], PossibleValues [1,2])
           ]

nakedSubsets2TestCaseResult :: Grid
nakedSubsets2TestCaseResult = [
  PossibleValues [1,2],
  PossibleValues [1,2],
  PossibleValues [3],
  CellValue 4,
  CellValue 5
                           ]

nakedSubsets2TestCase = testCase "Single naked pair" $ assertEqual "Candidates removed" nakedSubsets2TestCaseResult eval
  where eval = nakedSubsetsNGroupN testBoard1d 2 0 nakedSubsets2TestCaseBoard

handleCellTestGroup = testGroup "hancleCell tests" [ handleCellTestCase, handleCellTestCaseNothing ]

-- handleCell :: Int -> [CellInfo] -> CellInfo -> [CellInfo]
handleCellTestCaseData = [
  (Position [1], 1, PossibleValues [1,2]),
  (Position [2], 2, PossibleValues [1,2]),
  (Position [3], 3, PossibleValues [1,2,3]),
  (Position [4], 4, CellValue 4),
  (Position [5], 5, CellValue 5)
                         ]
handleCellTestCaseExpected = [
  (Position [1], 1, PossibleValues [1,2]),
  (Position [2], 2, PossibleValues [1,2]),
  (Position [3], 3, PossibleValues [3]),
  (Position [4], 4, CellValue 4),
  (Position [5], 5, CellValue 5)
                             ]

handleCellTestCase = testCase "handleCell basic test" $ assertEqual "" handleCellTestCaseExpected eval
  where eval = handleCell 2 handleCellTestCaseData (Position [2], 2, PossibleValues [1,2])


handleCellTestCaseNothingData = [
  (Position [1], 1, PossibleValues [1,2]),
  (Position [2], 2, PossibleValues [1,3]),
  (Position [3], 3, PossibleValues [1,2,3]),
  (Position [4], 4, CellValue 4),
  (Position [5], 5, CellValue 5)
                                ]
handleCellTestCaseNothing = testCase "handleCell test no nacked" $ assertEqual "" handleCellTestCaseNothingData handleCellTestCaseNothingData

handleGroupTestData = [
  (Position [1], 1, PossibleValues [1,2]),
  (Position [2], 2, PossibleValues [1,2]),
  (Position [3], 3, PossibleValues [1,2,3,4,5]),
  (Position [4], 4, PossibleValues [4,5]),
  (Position [5], 5, PossibleValues [5]),
  (Position [6], 6, CellValue 6)
                      ]

handleGroupTestExpected = [
  (Position [1], 1, PossibleValues [1,2]),
  (Position [2], 2, PossibleValues [1,2]),
  (Position [3], 3, PossibleValues [3]),
  (Position [4], 4, PossibleValues [4,5]),
  (Position [5], 5, PossibleValues [5]),
  (Position [6], 6, CellValue 6)
                          ]

handleGroupTestCase = testCase "handle group test" $ assertEqual "" handleGroupTestExpected eval
  where eval = handleGroup 2 handleGroupTestData


module Algs.NakedSubsetsTest (tests) where

import ClassicBoard
import Board
import Grid
import Test.Tasty
import Test.Tasty.HUnit
import TestBoard1d
import Algs.NakedSubsets

tests = testGroup "NakedSubsets algorithm tests"  [ nakedSubsets2TestGroup, handleCellTestGroup, handleGroupTestGroup ] 

nakedSubsets2TestGroup = testGroup "nakedSubsets2 tests" [ nakedSubsets2TestCase, nakedPairClassicClassicCaseA  ]

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


nakedPairClassicClassicCaseASource = readGridWith (updatePossibleValues classicBoard . classicInit)
  "7..849.3.928135..64..267.896427839513974516288156923..2.4516.931....8.6.5....4.1."

nakedPairClassicClassicCaseAResult = updateGridWithValues classicBoard nakedPairClassicClassicCaseASource [
  (Position [2,8], PossibleValues [7]),
  (Position [7,1], PossibleValues [1,2]),
  (Position [9,1], PossibleValues [5]),
  (Position [7,2], PossibleValues [4]),
  (Position [7,3], PossibleValues [1]),
  (Position [7,8], PossibleValues [4,5]),
  (Position [9,8], PossibleValues [4,5]),
  (Position [2,9], PossibleValues [3,6]),
  (Position [7,9], PossibleValues [8]),
  (Position [9,9], PossibleValues [2])

                                                                                                          ]
nakedPairClassicClassicCaseA = testCase "Naked pair classic test A" $ assertEqual "" nakedPairClassicClassicCaseAResult eval
  where eval = nakedSubsetsN classicBoard 2 nakedPairClassicClassicCaseASource


handleCellTestGroup = testGroup "hancleCell tests" [ handleCellTestCase, handleCellTestCaseNothing, handleCellTestCaseB ]

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

handleCellTestCaseB = testCase "handle cell test B" $ assertEqual "" expected eval
  where 
    expected = [
      (Position [1], 1, CellValue 1),
      (Position [2], 2, PossibleValues [7]),
      (Position [3], 3, PossibleValues [3,9]),
      (Position [4], 4, PossibleValues [3,9]),
      (Position [5], 5, PossibleValues [2,7]),
      (Position [6], 6, CellValue 8),
      (Position [7], 7, PossibleValues [2,4,5,7]),
      (Position [8], 8, CellValue 6),
      (Position [9], 9, PossibleValues [2,4,5,7])
               ]
    eval = handleCell 2 [
      (Position [1], 1, CellValue 1),
      (Position [2], 2, PossibleValues [3,7]), -- only one to update
      (Position [3], 3, PossibleValues [3,9]),
      (Position [4], 4, PossibleValues [3,9]),
      (Position [5], 5, PossibleValues [2,7]),
      (Position [6], 6, CellValue 8),
      (Position [7], 7, PossibleValues [2,4,5,7]),
      (Position [8], 8, CellValue 6),
      (Position [9], 9, PossibleValues [2,4,5,7])
                         ]
      (Position [3], 3, PossibleValues [3,9])

handleGroupTestGroup = testGroup "handleGroup tests"  [ handleGroupTestCase, handleGroupTestCaseB, handleGroupTestCaseC ]
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

handleGroupTestCaseB = testCase "handle group test B" $ assertEqual "" expected eval
  where 
    expected = [
      (Position [1], 1, CellValue 7),
      (Position [2], 2, PossibleValues [5, 6]),
      (Position [3], 3, PossibleValues [1, 6]),
      (Position [4], 4, CellValue 8),
      (Position [5], 5, CellValue 4),
      (Position [6], 6, CellValue 9),
      (Position [7], 7, PossibleValues [1, 2, 5]),
      (Position [8], 8, CellValue 3),
      (Position [9], 9, PossibleValues [2, 5])
               ]
    eval = handleGroup 2 expected

handleGroupTestCaseC = testCase "handle group test C" $ assertEqual "" expected eval
  where 
    expected = [
      (Position [1], 1, CellValue 1),
      (Position [2], 2, PossibleValues [7]), -- First iteration
      (Position [3], 3, PossibleValues [3,9]),
      (Position [4], 4, PossibleValues [3,9]),
      (Position [5], 5, PossibleValues [2,7]),
      (Position [6], 6, CellValue 8),
      (Position [7], 7, PossibleValues [4,5]), -- Second iteration
      (Position [8], 8, CellValue 6),
      (Position [9], 9, PossibleValues [4,5])
               ]
    eval = handleGroup 2 [
      (Position [1], 1, CellValue 1),
      (Position [2], 2, PossibleValues [3,7]), -- only one to update
      (Position [3], 3, PossibleValues [3,9]),
      (Position [4], 4, PossibleValues [3,9]),
      (Position [5], 5, PossibleValues [2,7]),
      (Position [6], 6, CellValue 8),
      (Position [7], 7, PossibleValues [2,4,5,7]),
      (Position [8], 8, CellValue 6),
      (Position [9], 9, PossibleValues [2,4,5,7])
                         ]

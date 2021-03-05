module BoardTest (tests) where
import Test.Tasty
import Test.Tasty.HUnit
import Grid
import Board
import ClassicBoard
import Data.Maybe

main = defaultMain tests

tests = testGroup "Board module tests"  [ removeCandidatesTests, updateGridWithValuesTestGroup, showClassicBoardTest ] 

removeCandidatesTests = testGroup "Remove Candidates Tests" [ removeCandidatesSimpleCase ]

simpleGrid = readGridWith classicInit "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
expected = updateGridWithValues classicBoard simpleGrid [
  (Position [1,1], PossibleValues [2,3,4,7,9]),
  (Position [2,1], PossibleValues [2,3,4,7,9]),
  (Position [3,1], PossibleValues [2,3,4,7,9]),
  (Position [5,1], PossibleValues [2,3,4,7,9]),
  (Position [7,1], PossibleValues [2,3,4,7,9])
  ]
group = head $ boardGroups classicBoard
actual = removeCandidates classicBoard simpleGrid group [1,5,6,8]
removeCandidatesSimpleCase = testCase "Simple rm candidates test case" $ assertEqual "Result is same" expected actual

updateGridWithValuesTestGroup = testGroup "update grid with values tests"  [ updateGridWithValuesTest, updateGridWithValuesNegativeTest ]

expectedUpdatedGrid = readGridWith classicInit "1234567892.....7.13.1....3.4.7.26...5.......36..87.4..73....8.58.5......99.4.1..."
actualUpdatedGrid = updateGridWithValues classicBoard simpleGrid [
  (Position [1,1], CellValue 1),
  (Position [1,2], CellValue 2),
  (Position [1,3], CellValue 3),
  (Position [1,4], CellValue 4),
  (Position [1,5], CellValue 5),
  (Position [1,6], CellValue 6),
  (Position [1,7], CellValue 7),
  (Position [1,8], CellValue 8),
  (Position [1,9], CellValue 9),
  (Position [2,1], CellValue 2),
  (Position [3,1], CellValue 3),
  (Position [4,1], CellValue 4),
  (Position [5,1], CellValue 5),
  (Position [6,1], CellValue 6),
  (Position [7,1], CellValue 7),
  (Position [8,1], CellValue 8),
  (Position [9,1], CellValue 9)
  ]

updateGridWithValuesTest = testCase "Update grid with values test case" $ assertEqual "Grid is same" expectedUpdatedGrid actualUpdatedGrid

updateGridWithValuesNegativeTest = testCase "Update grid with values test negative case" ((simpleGrid /= actualUpdatedGrid) @? "Grid is not same")

showClassicBoardTest = testCase "Show classic board test" $ assertEqual "" expected actual
  where
    expected = "Dim: 2, Size: 9, Groups: 27"
    actual = show classicBoard 

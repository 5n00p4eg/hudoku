import Test.Tasty
import Test.Tasty.HUnit
import Grid
import Board
import ClassicBoard
import Data.Maybe
import qualified BoardTest

main = defaultMain tests

tests = testGroup "All tests"  [basicTests, BoardTest.tests ]
basicTests = testGroup "Basic tests"  [ simplestTestCase, mediumTestCase, harderTestCase ] 

-- simple
simpleSolver = updatePossibleValues classicBoard . refreshGridValues 
simpleGrid = readGridWith classicInit "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
simplestTestCase = testCase "Simplest classic grid solved" $ assertBool "Not solved" $ isGridSolvedWith simpleGrid simpleSolver

--medium
mediumSolver = refreshGridValues . updateUniqueValues classicBoard . updatePossibleValues classicBoard
mediumGrid = readGridWith classicInit ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
mediumTestCase = testCase "Medium classic grid solved" $ assertBool "Not solved" $ isGridSolvedWith mediumGrid mediumSolver

-- harder
harderGrid = readGridWith classicInit $ concat [
  "5...1.7..",
  "8........",
  "2....35..",
  "..1.9..6.",
  "3..851..7",
  ".8..4.3..",
  "..51....9",
  "........3",
  "..4.3...2"
  ]
harderTestCase = testCase "Harder classic grid solved" $ assertBool "Not solved" $ isGridSolvedWith harderGrid mediumSolver

-- helpers
classicGridSolved = gridSolved  classicBoard
classicInitPossibleValues = initPossibleValues classicBoard
isGridSolvedWith :: Grid -> (Grid -> Grid) -> Bool
isGridSolvedWith g f = classicGridSolved $ recursiveUpdateWith f g

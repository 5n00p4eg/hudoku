module TestSuite
  where
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.HUnit
import Grid
import Board
import ClassicBoard
import Data.Maybe

main = defaultMain tests

tests = testGroup "Basic tests"  [ simplestTestCase ] 

test_grid = fromJust $ readGrid "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."

solved_grid = recursiveUpdate classic_board (initPossibleValues classic_board test_grid)
is_solved = gridSolved classic_board solved_grid

simplestTestCase = testCase "Simplest classic grid solved" $ assertBool "Not solved"  is_solved


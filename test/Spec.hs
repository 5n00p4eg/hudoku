import Test.Tasty
import Test.Tasty.HUnit
import Grid
import Board
import ClassicBoard
import Data.Maybe

main = defaultMain tests

tests = testGroup "Basic tests"  [ simplestTestCase, harderTestCase ] 

-- simple
testGrid = fromJust $ readGrid "...1.5.68......7.19.1....3...7.26...5.......3...87.4...3....8.51.5......79.4.1..."
isSolved = classicGridSolved $ classicRecursiveUpdate $ classicInitPossibleValues testGrid 
simplestTestCase = testCase "Simplest classic grid solved" $ assertBool "Not solved"  isSolved

-- harder
harderGrid = fromJust $ readGrid $ concat [
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

-- harderGrid = fromJust $ readGrid "5...1..7..8........2....35..751392.6.346851..7982.4.3..63512...9.28.....3.74.3...2"
isHarderSolved = classicGridSolved $ classicRecursiveUpdate $ classicInitPossibleValues harderGrid
harderTestCase = testCase "Harder classic grid solved" $ assertBool "Not solved" isHarderSolved



-- test_grid = fromJust $ readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."



-- helpers
classicGridSolved = gridSolved  classic_board
classicRecursiveUpdate = recursiveUpdate classic_board
classicInitPossibleValues = initPossibleValues classic_board

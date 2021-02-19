module TestBoard1d where

import Board
import Grid

testBoard1dSize = 5
testBoard1d = Board 1 5 testBoard1dGroups testBoard1dPositions
testBoardInit = initPossibleValues testBoard1d

testBoard1dGroups :: [Group]
testBoard1dGroups = [map (\x -> Position [x]) [1..testBoard1dSize]]

testBoard1dPositions :: PositionList 
testBoard1dPositions = map (\x -> (x, Position [x])) [1..testBoard1dSize]


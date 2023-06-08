module Game where

import Control.Monad
import Board
import Grid
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

type Game a = StateT Grid (ReaderT Board (Identity)) a

runGame :: Board -> Grid -> Game a -> (a, Grid)
runGame board grid action = runIdentity $ runReaderT (runStateT (action) grid) board

execGame :: Board -> Grid -> Game a -> Grid
execGame board grid action = runIdentity $ runReaderT (execStateT (action) grid) board

evalGame :: Board -> Grid -> Game a -> a
evalGame board grid action = runIdentity $ runReaderT (evalStateT (action) grid) board

gameSolved :: Game Bool
gameSolved = do
    board <- ask
    grid <- get
    return $ gridSolved board grid

initGame :: Board -> Game()
initGame board = do
    let size = length (boardPositionList board)
    let cells = take size ( repeat EmptyCellVallue)
    let grid = initPossibleValues' board cells
    put grid

fillGrid :: Grid -> Game()
fillGrid g = do
    put g

initPossibleValues :: Game ()
initPossibleValues = do
    grid <- get
    board <- ask
    put $ Board.initPossibleValues' board grid
    -- return ()

getGrid :: Game(Grid)
getGrid = do
    grid <- get
    return grid

getBoard :: Game(Board)
getBoard = ask

setGrid g = do
    put g

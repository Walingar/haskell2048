{-# LANGUAGE InstanceSigs #-}

module Game
  ( startGame
  ) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Foldable (forM_)
import Data.IORef (IORef, newIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random (newStdGen, randomRs)

data GameState =
  InProgress

data Move
  = TopMove
  | LeftMove
  | RightMove
  | DownMove

newtype Field =
  Field (V.Vector (MU.IOVector Int))

data GameData = GameData
  { field  :: Field
  , score  :: IORef Integer
  , state  :: IORef GameState
  , logger :: Field -> IO ()
  }

toVector :: [Int] -> IO (MU.IOVector Int)
toVector list = VU.thaw $ VU.fromList list

toField :: [[Int]] -> IO Field
toField list = do
  lists <- traverse toVector list
  return $ Field $ V.fromList lists

emptyField :: IO Field
emptyField =
  let line = replicate 5 0
   in toField (replicate 5 line)

joinCell :: (Int, Int) -> (Int, Int) -> ReaderT GameData IO ()
joinCell (x1, y1) (x2, y2) = do
  GameData {field = Field curField} <- ask
  let rowFrom = curField V.! x1
  elFrom <- MU.read rowFrom y1
  let rowTo = curField V.! x2
  elTo <- MU.read rowTo y2
  when (elTo == elFrom) $ do
    MU.write rowFrom y1 0
    MU.write rowTo y2 (2 * elTo)
    return ()
  when (elTo == 0) $ do
    MU.write rowFrom y1 0
    MU.write rowTo y2 elFrom
    return ()

moveImpl :: Move -> ReaderT GameData IO ()
moveImpl TopMove =
  forM_ [1 .. 4] $ \i ->
    forM_ [1 .. i] $ \moveCount ->
      forM_ [0 .. 4] $ \j ->
        joinCell (i - moveCount + 1, j) (i - moveCount, j)
moveImpl RightMove =
  forM_ [3,2 .. 0] $ \j ->
    forM_ [1 .. (4 - j)] $ \moveCount ->
      forM_ [0 .. 4] $ \i ->
        joinCell (i, j + moveCount - 1) (i, j + moveCount)
moveImpl DownMove =
  forM_ [3,2 .. 0] $ \i ->
    forM_ [1 .. (4 - i)] $ \moveCount ->
      forM_ [0 .. 4] $ \j ->
        joinCell (i + moveCount - 1, j) (i + moveCount, j)
moveImpl LeftMove =
  forM_ [1 .. 4] $ \j ->
    forM_ [1 .. j] $ \moveCount ->
      forM_ [0 .. 4] $ \i -> do
        lift $ putStrLn (show (i, j - moveCount + 1) ++ " " ++ show (i, j - moveCount))
        joinCell (i, j - moveCount + 1) (i, j - moveCount)

move :: ReaderT GameData IO ()
move = do
  userAction <- lift getLine
  case userAction of
    "t" -> moveImpl TopMove
    "r" -> moveImpl RightMove
    "d" -> moveImpl DownMove
    "l" -> moveImpl LeftMove
    "s" -> undefined
    _   -> error ("Unexpected user action: " ++ userAction)
  turn

turn :: ReaderT GameData IO ()
turn = do
  GameData {field = curField, logger = curLogger} <- ask
  newRandomCell
  lift $ curLogger curField
  move

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _      = error "Expected list with 2 elements"

randomCellId :: IO (Int, Int)
randomCellId = fmap tuplify $ take 2 . randomRs (0, 4) <$> newStdGen

newRandomCell :: ReaderT GameData IO ()
newRandomCell = do
  GameData {field = (Field vector)} <- ask
  (i, j) <- lift randomCellId
  let row = vector V.! i
  el <- MU.read row j
  if el == 0
    then MU.write row j 2
    else newRandomCell

simpleLogger :: Field -> IO ()
simpleLogger (Field vector) =
  forM_ [0 .. 4] $ \i -> do
    let row = vector V.! i
    forM_ [0 .. 4] $ \j -> do
      el <- MU.read row j
      putStr (show el ++ " ")
    putStrLn ""

startGame :: IO ()
startGame = do
  curScore <- newIORef 0 :: IO (IORef Integer)
  curField <- emptyField
  curState <- newIORef InProgress
  runReaderT turn (GameData {field = curField, score = curScore, state = curState, logger = simpleLogger})

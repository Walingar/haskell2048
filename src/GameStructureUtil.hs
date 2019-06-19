module GameStructureUtil
  ( toVector
  , toField
  , emptyField
  , equal
  , checkPredicate
  , vectorToList
  , fieldToList
  , fieldColumnToList
  , newRandomCell
  , initGameData
  ) where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import GameStructure (Field (..), GameData (..), GameState (..), maxSize)
import System.Random (newStdGen, randomRs)

toVector :: [Int] -> IO (MU.IOVector Int)
toVector list = VU.thaw $ VU.fromList list

toField :: [[Int]] -> IO Field
toField list = do
  lists <- traverse toVector list
  return $ Field $ V.fromList lists

emptyField :: IO Field
emptyField = do
  let line = replicate maxSize 0
  curField <- toField (replicate maxSize line)
  newRandomCell curField
  newRandomCell curField
  return curField

equal :: Field -> Field -> IO Bool
equal (Field a) (Field b) = do
  ans <- newIORef True
  forM_ [0 .. maxSize - 1] $ \i -> do
    let rowA = a V.! i
    let rowB = b V.! i
    forM_ [0 .. maxSize - 1] $ \j -> do
      elA <- MU.read rowA j
      elB <- MU.read rowB j
      when (elA /= elB) $ writeIORef ans False
  readIORef ans

checkPredicate :: Field -> (Int -> Bool) -> IO Bool
checkPredicate (Field a) f = do
  ans <- newIORef False
  forM_ [0 .. maxSize - 1] $ \i -> do
    let rowA = a V.! i
    forM_ [0 .. maxSize - 1] $ \j -> do
      elA <- MU.read rowA j
      when (f elA) $ writeIORef ans True
  readIORef ans

vectorToList :: MU.IOVector Int -> IO [Int]
vectorToList vector = do
  immutableVector <- VU.freeze vector
  return $ VU.toList immutableVector

fieldToList :: Field -> IO [[Int]]
fieldToList (Field vector) = do
  res <- newIORef []
  forM_ [0 .. maxSize - 1] $ \i -> do
    let row = vector V.! i
    el <- vectorToList row
    modifyIORef res (\x -> el : x)
  list <- readIORef res
  return $ reverse list

fieldColumnToList :: Field -> Int -> IO [Int]
fieldColumnToList (Field vector) j = do
  res <- newIORef []
  forM_ [0 .. maxSize - 1] $ \i -> do
    let row = vector V.! i
    el <- MU.read row j
    modifyIORef res (\x -> el : x)
  list <- readIORef res
  return $ reverse list

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _      = error "Expected list with 2 elements"

randomCellId :: IO (Int, Int)
randomCellId = fmap tuplify $ take 2 . randomRs (0, maxSize - 1) <$> newStdGen

randomCell :: IO Int
randomCell = do
  randomCellProb <- randomCell'
  return $
    if randomCellProb <= 10
      then 4
      else 2
  where
    randomCell' :: IO Int
    randomCell' = fmap head $ take 1 . randomRs (0, 100) <$> newStdGen

newRandomCell :: Field -> IO ()
newRandomCell curField@(Field vector) = do
  randomCellValue <- randomCell
  (i, j) <- randomCellId
  let row = vector V.! i
  el <- MU.read row j
  if el == 0
    then MU.write row j randomCellValue
    else newRandomCell curField

initGameData :: (GameData -> IO ()) -> IO GameData
initGameData curLogger = do
  curScore <- newIORef 0 :: IO (IORef Int)
  curNewCellCount <- newIORef 2 :: IO (IORef Int)
  curGameState <- newIORef InProgress
  curField <- emptyField
  return
    GameData
      { field = curField
      , score = curScore
      , logger = curLogger
      , newCellCount = curNewCellCount
      , gameState = curGameState
      }

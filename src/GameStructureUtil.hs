module GameStructureUtil
  ( toVector
  , toField
  , emptyField
  , equal
  , checkPredicate
  , vectorToList
  , fieldToList
  , fieldColumnToList
  ) where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import GameStructure (Field (..), maxSize)

toVector :: [Int] -> IO (MU.IOVector Int)
toVector list = VU.thaw $ VU.fromList list

toField :: [[Int]] -> IO Field
toField list = do
  lists <- traverse toVector list
  return $ Field $ V.fromList lists

emptyField :: IO Field
emptyField =
  let line = replicate maxSize 0
   in toField (replicate maxSize line)

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

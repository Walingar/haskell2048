module GameStructure
  ( Move(..)
  , Field(..)
  , GameData(..)
  , GameState(..)
  , maxSize
  ) where

import Data.IORef (IORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU

data Move
  = UpMove
  | LeftMove
  | RightMove
  | DownMove

data GameState
  = InProgress
  | Win
  | Lose

newtype Field =
  Field (V.Vector (MU.IOVector Int))

data GameData = GameData
  { field        :: Field
  , score        :: IORef Int
  , logger       :: GameData -> IO ()
  , newCellCount :: IORef Int
  , gameState    :: IORef GameState
  }

maxSize :: Int
maxSize = 4

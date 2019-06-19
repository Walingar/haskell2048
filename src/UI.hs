module UI
  ( draw
  ) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU
import Game (moveImpl, simpleLogger, turn)
import GameStructure (Field (..), GameData (..), Move (..), maxSize)
import GameStructureUtil (emptyField)
import Graphics.Gloss (Display (..))
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Data.Color (Color, makeColorI)
import Graphics.Gloss.Data.Picture (Picture (..), pictures, scale, translate)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), playIO)
import System.Exit (exitSuccess)

window :: Display
window = InWindow "2048" (500, 500) (0, 0)

getImgName :: Int -> String
getImgName x = "./image/" ++ show x ++ ".bmp"

scorePicture :: Int -> Picture
scorePicture x = scale 0.2 0.2 $ translate (-400) 800 $ Text $ "Score: " ++ show x

render :: GameData -> IO Picture
render GameData {score = curScoreRef, field = Field vector} = do
  curScore <- readIORef curScoreRef
  picturesListRef <- newIORef [scorePicture curScore]
  forM_ [0 .. maxSize - 1] $ \i -> do
    let row = vector V.! i
    forM_ [0 .. maxSize - 1] $ \j -> do
      el <- MU.read row j
      curPictureRaw <- loadBMP $ getImgName el
      let curPicture =
            scale 0.5 0.5 $
            translate
              (75 + (fromIntegral j - 2) * 150)
              ((-75) + (2 - fromIntegral i) * 150)
              curPictureRaw
      modifyIORef picturesListRef (\x -> curPicture : x)
  picturesList <- readIORef picturesListRef
  return $ pictures picturesList

frame :: Float -> GameData -> IO GameData
frame _ = return

gameImpl :: Move -> GameData -> IO GameData
gameImpl move gameData = do
  runReaderT (turn move) gameData
  return gameData

game :: Event -> GameData -> IO GameData
game (EventKey (Char 'w') Down _ _) gameData = gameImpl UpMove gameData
game (EventKey (Char 'a') Down _ _) gameData = gameImpl LeftMove gameData
game (EventKey (Char 's') Down _ _) gameData = gameImpl DownMove gameData
game (EventKey (Char 'd') Down _ _) gameData = gameImpl RightMove gameData
game (EventKey (Char 'c') Down _ _) _        = undefined
game (EventKey (Char 'l') Down _ _) _        = undefined
game (EventKey (Char 'q') Down _ _) _        = exitSuccess
game _ gameData                              = return gameData

bgColor :: Color
bgColor = makeColorI 187 173 162 1

draw :: IO ()
draw = do
  curScore <- newIORef 0 :: IO (IORef Int)
  curNewCellCount <- newIORef 2 :: IO (IORef Int)
  curField <- emptyField
  let gameData =
        GameData
          { field = curField
          , score = curScore
          , logger = simpleLogger
          , newCellCount = curNewCellCount
          }
  playIO window bgColor 60 gameData render game frame

module UI
  ( draw
  ) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU
import Game (simpleLogger, turn)
import GameStructure (Field (..), GameData (..), GameState (..), Move (..), maxSize)
import GameStructureUtil (initGameData)
import Graphics.Gloss (Display (..))
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Data.Color (Color, makeColorI)
import Graphics.Gloss.Data.Picture (Picture (..), pictures, scale, translate)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), playIO)
import SaveGameData (loadData, saveData)
import System.Exit (exitSuccess)

window :: Display
window = InWindow "2048" (500, 500) (0, 0)

getImgName :: Int -> String
getImgName x = "./image/" ++ show x ++ ".bmp"

scaledText :: String -> Picture
scaledText s = scale 0.2 0.2 $ Text s

scorePicture :: Int -> Picture
scorePicture x = translate (-75) 160 $ scaledText $ "Score: " ++ show x

render :: GameData -> IO Picture
render GameData {score = curScoreRef, field = Field vector, gameState = curGameStateRef} = do
  curGameState <- readIORef curGameStateRef
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
  case curGameState of
    InProgress -> return ()
    Win -> modifyIORef picturesListRef (\x -> x ++ [translate (-75) 0 $ scaledText "You Win!"])
    Lose -> modifyIORef picturesListRef (\x -> x ++ [translate (-75) 0 $ scaledText "You Lose :("])
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
game (EventKey (Char 'l') Down _ _) gameData = do
  loadedData <- loadData gameData
  putStrLn "Game data has been loaded!"
  return loadedData
game (EventKey (Char 'c') Down _ _) gameData = do
  saveData gameData
  putStrLn "Game data has been saved!"
  return gameData
game (EventKey (Char 'q') Down _ _) _ = exitSuccess
game (EventKey (Char 'r') Down _ _) _ = initGameData simpleLogger
game _ gameData = return gameData

bgColor :: Color
bgColor = makeColorI 187 173 162 1

draw :: IO ()
draw = do
  curGameData <- initGameData simpleLogger
  playIO window bgColor 60 curGameData render game frame

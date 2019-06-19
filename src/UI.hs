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
import Graphics.Gloss.Interface.IO.Interact (Controller, Event (..), Key (..), KeyState (..),
                                             interactIO)
import SaveGameData (loadData, saveData)
import System.Exit (exitSuccess)

data MemoizedData = MemoizedData
  { gameData' :: GameData
  , picture'  :: Picture
  }

window :: Display
window = InWindow "2048" (500, 500) (0, 0)

getImgName :: Int -> String
getImgName x = "./image/" ++ show x ++ ".bmp"

scaledText :: String -> Picture
scaledText s = scale 0.2 0.2 $ Text s

scorePicture :: Int -> Picture
scorePicture x = translate (-75) 160 $ scaledText $ "Score: " ++ show x

render' :: GameData -> IO Picture
render' GameData {score = curScoreRef, field = Field vector, gameState = curGameStateRef} = do
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

generateMemoizedData :: GameData -> IO MemoizedData
generateMemoizedData gameData = do
  curPicture <- render' gameData
  return $ MemoizedData {gameData' = gameData, picture' = curPicture}

gameImpl :: Move -> MemoizedData -> IO MemoizedData
gameImpl move MemoizedData {gameData' = gameData} = do
  runReaderT (turn move) gameData
  generateMemoizedData gameData

game :: Event -> MemoizedData -> IO MemoizedData
game (EventKey (Char 'w') Down _ _) gameData = gameImpl UpMove gameData
game (EventKey (Char 'a') Down _ _) gameData = gameImpl LeftMove gameData
game (EventKey (Char 's') Down _ _) gameData = gameImpl DownMove gameData
game (EventKey (Char 'd') Down _ _) gameData = gameImpl RightMove gameData
game (EventKey (Char 'l') Down _ _) MemoizedData {gameData' = gameData} = do
  loadedData <- loadData gameData
  putStrLn "Game data has been loaded!"
  generateMemoizedData loadedData
game (EventKey (Char 'c') Down _ _) memoizedData@MemoizedData {gameData' = gameData} = do
  saveData gameData
  putStrLn "Game data has been saved!"
  return memoizedData
game (EventKey (Char 'q') Down _ _) _ = exitSuccess
game (EventKey (Char 'r') Down _ _) _ = do
  newGameData <- initGameData simpleLogger
  curPicture <- render' newGameData
  return $ MemoizedData {picture' = curPicture, gameData' = newGameData}
game _ gameData = return gameData

render :: MemoizedData -> IO Picture
render MemoizedData {picture' = curPicture} = return curPicture

bgColor :: Color
bgColor = makeColorI 187 173 162 1

f :: Controller -> IO ()
f _ = return ()

draw :: IO ()
draw = do
  curGameData <- initGameData simpleLogger
  curPicture <- render' curGameData
  interactIO
    window
    bgColor
    (MemoizedData {gameData' = curGameData, picture' = curPicture})
    render
    game
    f

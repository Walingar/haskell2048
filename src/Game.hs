{-# LANGUAGE InstanceSigs #-}

module Game
  ( startGame
  ) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Foldable (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import System.IO (BufferMode (..), hSetBuffering, stdin)
import System.Random (newStdGen, randomRs)

data Move
  = TopMove
  | LeftMove
  | RightMove
  | DownMove

newtype Field =
  Field (V.Vector (MU.IOVector Int))

data GameData = GameData
  { field  :: Field
  , score  :: IORef Int
  , logger :: GameData -> IO ()
  }

maxSize :: Int
maxSize = 4

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

isWin :: Field -> IO Bool
isWin curField = checkPredicate curField (>= 2048)

hasEmpty :: Field -> IO Bool
hasEmpty curField = checkPredicate curField (== 0)

isLose :: Field -> IO Bool
isLose curField@(Field vector) = do
  ans <- newIORef True
  fieldHasEmpty <- hasEmpty curField
  when fieldHasEmpty $ writeIORef ans False
  forM_ [0 .. maxSize - 1] $ \i -> do
    let row = vector V.! i
    forM_ [0 .. maxSize - 1] $ \j -> do
      el <- MU.read row j
      when (j > 0) $ do
        el' <- MU.read row (j - 1)
        when (el == el') $ writeIORef ans False
      when (j < maxSize - 1) $ do
        el' <- MU.read row (j + 1)
        when (el == el') $ writeIORef ans False
      when (i > 0) $ do
        let row' = vector V.! (i - 1)
        el' <- MU.read row' j
        when (el == el') $ writeIORef ans False
      when (i < maxSize - 1) $ do
        let row' = vector V.! (i + 1)
        el' <- MU.read row' j
        when (el == el') $ writeIORef ans False
  readIORef ans

slideRowLeft :: [Int] -> IORef Int -> IO [Int]
slideRowLeft [] _ = return []
slideRowLeft [x] _ = return [x]
slideRowLeft (x:y:zs) curScoreRef
  | x == 0 = do
    left <- slideRowLeft (y : zs) curScoreRef
    return $ left ++ [0]
  | y == 0 = do
    left <- slideRowLeft (x : zs) curScoreRef
    return $ left ++ [0]
  | x == y = do
    modifyIORef curScoreRef (\z -> z + x + y)
    center <- slideRowLeft zs curScoreRef
    return $ (x + y) : center ++ [0]
  | otherwise = do
    right <- slideRowLeft (y : zs) curScoreRef
    return $ x : right

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

moveColumns :: Bool -> ReaderT GameData IO ()
moveColumns toLeft = do
  GameData {field = curField@(Field vector), score = curScoreRef} <- ask
  lift $
    forM_ [0 .. maxSize - 1] $ \j -> do
      curColumn <- fieldColumnToList curField j
      movedColumn <-
        if toLeft
          then do
            slided <- slideRowLeft curColumn curScoreRef
            toVector slided
          else do
            slided <- slideRowLeft (reverse curColumn) curScoreRef
            toVector $ reverse slided
      forM_ [0 .. maxSize - 1] $ \i -> do
        let row = vector V.! i
        movedEl <- MU.read movedColumn i
        MU.write row j movedEl

moveRows :: Bool -> ReaderT GameData IO ()
moveRows toLeft = do
  GameData {field = (Field curField), score = curScoreRef} <- ask
  lift $
    forM_ [0 .. maxSize - 1] $ \i -> do
      let row = curField V.! i
      curRow <- vectorToList row
      movedRow <-
        if toLeft
          then do
            slided <- slideRowLeft curRow curScoreRef
            toVector slided
          else do
            slided <- slideRowLeft (reverse curRow) curScoreRef
            toVector $ reverse slided
      forM_ [0 .. maxSize - 1] $ \j -> do
        movedEl <- MU.read movedRow j
        MU.write row j movedEl

moveImpl :: Move -> ReaderT GameData IO ()
moveImpl TopMove   = moveColumns True
moveImpl RightMove = moveRows False
moveImpl DownMove  = moveColumns False
moveImpl LeftMove  = moveRows True

move :: ReaderT GameData IO ()
move = do
  userAction <- lift getChar
  lift $ putStrLn ""
  case userAction of
    'w' -> moveImpl TopMove
    'd' -> moveImpl RightMove
    's' -> moveImpl DownMove
    'a' -> moveImpl LeftMove
    'c' -> undefined
    _   -> error ("Unexpected user action: " ++ show userAction)

turn :: Bool -> ReaderT GameData IO ()
turn newCell = do
  curData@GameData {field = curField, logger = curLogger} <- ask
  curIsWin <- lift $ isWin curField
  curIsLose <- lift $ isLose curField
  when curIsWin $ do
    lift $ putStrLn "You win!"
    return ()
  when curIsLose $ do
    lift $ putStrLn "You lose! :("
    return ()
  when (not curIsWin && not curIsLose) $ do
    canAddCell <- lift $ hasEmpty curField
    when (newCell && canAddCell) newRandomCell
    savedFieldList <- lift $ fieldToList curField
    savedField <- lift $ toField savedFieldList
    lift $ curLogger curData
    move
    eq <- lift $ equal savedField curField
    turn (not eq)

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

newRandomCell :: ReaderT GameData IO ()
newRandomCell = do
  GameData {field = (Field vector)} <- ask
  randomCellValue <- lift randomCell
  (i, j) <- lift randomCellId
  let row = vector V.! i
  el <- MU.read row j
  if el == 0
    then MU.write row j randomCellValue
    else newRandomCell

simpleLogger :: GameData -> IO ()
simpleLogger GameData {field = Field vector, score = curScoreRef} = do
  curScore <- readIORef curScoreRef
  putStrLn $ "Your score is " ++ show curScore
  forM_ [0 .. maxSize - 1] $ \i -> do
    let row = vector V.! i
    forM_ [0 .. maxSize - 1] $ \j -> do
      el <- MU.read row j
      putStr (show el ++ " ")
    putStrLn ""

startGame :: IO ()
startGame = do
  hSetBuffering stdin NoBuffering
  curScore <- newIORef 0 :: IO (IORef Int)
  curField <- emptyField
  runReaderT (turn True) (GameData {field = curField, score = curScore, logger = simpleLogger})

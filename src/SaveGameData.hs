module SaveGameData
  ( saveData
  , loadData
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (forM_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Void (Void)
import GameStructure (Field (..), GameData (..), maxSize)
import Text.Megaparsec (Parsec, eof, parse)
import Text.Megaparsec.Char (char, digitChar, eol, space, string)
import Text.Megaparsec.Error (errorBundlePretty)

saveData :: GameData -> IO ()
saveData curData = do
  dataLog <- saveData' curData
  writeFile "save" dataLog
  where
    saveData' :: GameData -> IO String
    saveData' GameData {score = curScoreRef, field = Field curField} = do
      curScore <- readIORef curScoreRef
      res <- newIORef ("Score: " ++ show curScore ++ "\n")
      forM_ [0 .. maxSize - 1] $ \i -> do
        let row = curField V.! i
        forM_ [0 .. maxSize - 1] $ \j -> do
          el <- MU.read row j
          modifyIORef res (\s -> concat [s, "Cell ", show i, " ", show j, ": ", show el, "\n"])
      readIORef res

type Parser = Parsec Void String

parseIntString :: Parser String
parseIntString = do
  ch <- fmap Left digitChar <|> fmap Right (pure ())
  case ch of
    Left dgt -> do
      suffix <- parseIntString
      return $ dgt : suffix
    Right _ -> return []

parseScore :: Parser Int
parseScore = do
  i <- string "Score:" *> space *> parseIntString
  _ <- eol
  return $ read i

parseCell :: Parser (Int, Int, Int)
parseCell = do
  i <- string "Cell" *> space *> parseIntString
  j <- space *> parseIntString
  el <- char ':' *> space *> parseIntString
  _ <- eol
  return (read i, read j, read el)

parseCells :: Parser [(Int, Int, Int)]
parseCells = do
  line <- fmap Left parseCell <|> fmap Right eof
  case line of
    Left cell -> do
      parsedTail <- parseCells
      return $ cell : parsedTail
    Right _ -> return []

data ParsedData = ParsedData
  { parsedField :: [(Int, Int, Int)]
  , parsedScore :: Int
  }

parseData :: Parser ParsedData
parseData = do
  curScore <- parseScore
  curField <- parseCells
  return $ ParsedData {parsedField = curField, parsedScore = curScore}

copyLoadedField :: [(Int, Int, Int)] -> Field -> IO ()
copyLoadedField [] _ = return ()
copyLoadedField ((i, j, el):xs) curField@(Field vector) = do
  let row = vector V.! i
  MU.write row j el
  copyLoadedField xs curField

loadData :: GameData -> IO GameData
loadData curGameData@GameData {score = curScoreRef, field = curField} = do
  input <- readFile "save"
  case parse parseData "parse_log" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right ParsedData {parsedField = curParsedField, parsedScore = curScore} -> do
      writeIORef curScoreRef curScore
      copyLoadedField curParsedField curField
  return curGameData

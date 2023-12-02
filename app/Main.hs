module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.IO(hFlush, stdout)
import System.Random
import System.Random.Stateful

data FlakeType = Flake1 | Flake2 | Flake3
  deriving (Enum, Bounded)

instance Uniform FlakeType where
  uniformM = uniformEnumM

instance Show FlakeType where
  show Flake1 = "❄"
  show Flake2 = "❅"
  show Flake3 = "❆"

data PileHeight =
    Pile1 | Pile2 | Pile3 | Pile4
  | Pile5 | Pile6 | Pile7 | Pile8
  deriving (Enum, Eq, Ord)

instance Show PileHeight where
  show Pile1 = "▁"
  show Pile2 = "▂"
  show Pile3 = "▃"
  show Pile4 = "▄"
  show Pile5 = "▅"
  show Pile6 = "▆"
  show Pile7 = "▇"
  show Pile8 = "█"

data Snowflake = Snowflake FlakeType Int Int Float Float Float Float

render :: [Snowflake] -> IO ()
render [] = mempty
render (Snowflake ft x y _ _ _ _ : rest) = do
  setCursorPosition x y
  putStr $ show ft
  render rest

runApp :: Int -> Int -> IO ()
runApp rows cols = do
  go []
  where
    go flakes = do
      clearScreen
      render flakes
      hFlush stdout
      threadDelay 10000
      gen <- newStdGen
      let new = fst $ runStateGen gen randomFlakes
      go $ new ++ update flakes

    randomFlakes :: StatefulGen g m => g -> m [Snowflake]
    randomFlakes g = do
      n <- uniformRM (0 :: Float, 1) g
      if n > 0.75
      then do
        f <- randomFlake g
        return [f]
      else return []

    randomFlake :: StatefulGen g m => g -> m Snowflake
    randomFlake g = do
      ft <- uniformM g
      y  <- uniformRM (0, cols-1) g
      ax <- uniformRM (0.0, 0.5) g
      ay <- uniformRM (-1, 1) g
      dx <- uniformRM (0.03, 0.05) g
      dy <- uniformRM (-0.02, 0.02) g
      return $ Snowflake ft 0 y ax ay dx dy

    update :: [Snowflake] -> [Snowflake]
    update = mapMaybe updateFlake

    updateFlake :: Snowflake -> Maybe Snowflake
    updateFlake f =
      if x < 0 || x >= rows
      then Nothing
      else Just $ Snowflake ft x (y `mod` cols) ax ay dx dy
      where
        Snowflake ft x y ax ay dx dy = moveFlake f

    moveFlake :: Snowflake -> Snowflake
    moveFlake (Snowflake ft x y ax ay dx dy)
      | abs dx >= abs dy && abs ax' >= 1 =
        Snowflake ft x' y' ax'' ay'' dx dy
      | abs dy >= abs dx && abs ay' >= 1 =
        Snowflake ft x' y' ax'' ay'' dx dy
      | otherwise = Snowflake ft x y ax' ay' dx dy
      where
        ax' = ax + dx
        ay' = ay + dy
        x' = x + truncate ax'
        y' = y + truncate ay'
        ax'' = ax' - fromIntegral (truncate ax' :: Int)
        ay'' = ay' - fromIntegral (truncate ay' :: Int)



setup :: IO ()
setup = do
  hideCursor
  useAlternateScreenBuffer
  hFlush stdout

teardown :: IO ()
teardown = do
  useNormalScreenBuffer
  showCursor
  hFlush stdout

main :: IO ()
main = do
  Just (rows, cols) <- getTerminalSize
  bracket_ setup teardown $
           runApp rows cols

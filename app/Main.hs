module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.IO(hFlush, stdout)
import System.Random
import System.Random.Stateful

updateDelay :: Int
updateDelay = 10000

updateDelaySecs :: Float
updateDelaySecs = fromIntegral updateDelay / 1000000.0

flakeRate :: Float
flakeRate = 0.05

windDrag :: Float
windDrag = 10.0 * updateDelaySecs

windIntensity :: Float
windIntensity = 10.0

windChangeRate :: Float
windChangeRate = 2*pi*updateDelaySecs / 5.0

flakeDropSpeed :: StatefulGen g m => g -> m Float
flakeDropSpeed g = do
  r <- uniformRM (-1, 1) g
  return $ 6 + r ^ (3::Int)

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

flakePos :: Snowflake -> (Int, Int)
flakePos (Snowflake _ x y _ _ _ _) = (x, y)

render :: Snowflake -> IO ()
render (Snowflake ft x y _ _ _ _) = do
  setCursorPosition x y
  putStr $ show ft

clearPos :: (Int, Int) -> IO ()
clearPos (x, y) = do
  setCursorPosition x y
  putChar ' '

runApp :: Int -> Int -> IO ()
runApp rows cols = do
  go 0.0 []
  where
    go wind flakes = do
      let (upds, flakes') = update ((sin wind)^(3 :: Int)) flakes
      gen <- newStdGen
      let new = runStateGen_ gen randomFlakes
      mapM_ clearPos upds
      mapM_ render $ new ++ filter (\f -> flakePos f `elem` upds) flakes'
      setCursorPosition 0 0
      hFlush stdout
      threadDelay updateDelay
      dWind <- randomRIO (-1.0, 1.0)
      go (wind + dWind * windChangeRate) $ new ++ flakes'

    randomFlakes :: StatefulGen g m => g -> m [Snowflake]
    randomFlakes g = do
      n <- bernoulli (flakeRate * updateDelaySecs) cols g
      replicateM n (randomFlake g)

    randomFlake :: StatefulGen g m => g -> m Snowflake
    randomFlake g = do
      ft <- uniformM g
      y  <- uniformRM (0, cols-1) g
      ax <- uniformRM (0.0, 0.5) g
      ay <- uniformRM (-1, 1) g
      dx <- flakeDropSpeed g
      dy <- uniformRM (-2.0, 2.0) g
      return $ Snowflake ft 0 y ax ay dx dy

    update :: Float -> [Snowflake] -> ([(Int, Int)], [Snowflake])
    update wind = foldr updates ([], [])
      where
        updates f (upds, fs)
          | x' < 0 || x' >= rows = (fxy : upds, fs)
          | fxy == fxy' = (upds, f' : fs)
          | otherwise = (fxy : fxy' : upds, f' : fs)
          where
            f' = moveFlake wind f
            fxy = flakePos f
            fxy'@(x', _) = flakePos f'

    moveFlake :: Float -> Snowflake -> Snowflake
    moveFlake wind (Snowflake ft x y ax ay dx dy)
      | abs dx >= abs dy && abs ax' >= 1 =
        Snowflake ft x' y' ax'' ay'' dx dy'
      | abs dy >= abs dx && abs ay' >= 1 =
        Snowflake ft x' y' ax'' ay'' dx dy'
      | otherwise = Snowflake ft x y ax' ay' dx dy'
      where
        ax' = ax + dx * updateDelaySecs
        ay' = ay + dy * updateDelaySecs
        x' = x + truncate ax'
        y' = (y + truncate ay') `mod` cols
        ax'' = ax' - fromIntegral (truncate ax' :: Int)
        ay'' = ay' - fromIntegral (truncate ay' :: Int)
        dy' = (1.0 - windDrag) * dy + windDrag * windIntensity * wind

bernoulli :: StatefulGen g m => Float -> Int -> g -> m Int
bernoulli p n = go n 0
  where
    go 0 acc _ = return acc
    go m acc g = do
      q <- uniformRM (0, 1) g
      go (m - 1) (acc + if q < p then 1 else 0) g

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

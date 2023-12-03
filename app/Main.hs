module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array.IArray
import Data.List
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

data Snowflake = Snowflake FlakeType !Int !Int !Float !Float !Float !Float

flakePos :: Snowflake -> (Int, Int)
flakePos (Snowflake _ x y _ _ _ _) = (x, y)

render :: Snowflake -> IO ()
render (Snowflake ft x y _ _ _ _) = do
  setCursorPosition x y
  putStr $ show ft

renderPile :: Int -> Int -> Int -> IO ()
renderPile rows col height =
  go (min (rows * 8) height) (rows-1)
  where
    go 0 _ = mempty
    go 1 h = setCursorPosition h col >> putChar '▁'
    go 2 h = setCursorPosition h col >> putChar '▂'
    go 3 h = setCursorPosition h col >> putChar '▃'
    go 4 h = setCursorPosition h col >> putChar '▄'
    go 5 h = setCursorPosition h col >> putChar '▅'
    go 6 h = setCursorPosition h col >> putChar '▆'
    go 7 h = setCursorPosition h col >> putChar '▇'
    go n h = do
      setCursorPosition h col
      putChar '█'
      go (n-8) (h-1)

clearPos :: (Int, Int) -> IO ()
clearPos (x, y) = do
  setCursorPosition x y
  putChar ' '

runApp :: Int -> Int -> IO ()
runApp rows cols = do
  go 0.0 (genArray (0, cols-1) (const 0)) []
  where
    go pWind piles flakes = do
      let wind = (sin pWind)^(3 :: Int)
      let (upds, lands, flakes') = update wind piles flakes
      let newPiles = smooth wind $ accum (+) piles $ map (\i -> (i,1)) lands
      gen <- newStdGen
      let new = runStateGen_ gen randomFlakes
      mapM_ clearPos upds
      mapM_ render $ new ++ filter (\f -> flakePos f `elem` upds) flakes'
      mapM_ (\i -> renderPile rows i $ newPiles ! i) [0..cols-1]
      setCursorPosition 0 0
      hFlush stdout
      threadDelay $ updateDelay
      dWind <- randomRIO (-1.0, 1.0)
      go (pWind + dWind * windChangeRate)
         newPiles
         (new ++ flakes')

    smooth :: Float -> Array Int Int -> Array Int Int
    smooth wind a =
      accum (+) a $ concat [ updates i | i <- [0..cols-1] ]
      where
        updates i
          | e <= left + 1 && e <= right + 1 = []
          | left < right = slideLeft
          | left > right = slideRight
          | wind >= 0 = slideRight
          | otherwise = slideLeft
          where
            e = a ! i
            iLeft = if i == 0 then cols-1 else i-1
            iRight = if i == cols-1 then 0 else i+1
            left = a ! iLeft
            right = a ! iRight
            slideLeft = [(i, -1), (iLeft, 1)]
            slideRight = [(i, -1), (iRight, 1)]

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

    update :: Float -> Array Int Int -> [Snowflake] -> ([(Int, Int)], [Int], [Snowflake])
    update wind piles = foldl' updates ([], [], [])
      where
        updates (upds, cs, fs) f
          | x' < 0 = undefined
          | x' >= rows - pHeight = (fxy : upds, y' : cs, fs)
          | fxy == fxy' = (upds, cs, f' : fs)
          | otherwise = (fxy : fxy' : upds, cs, f' : fs)
          where
            f' = moveFlake wind f
            fxy = flakePos f
            fxy'@(x', y') = flakePos f'
            pHeight = ((piles ! y') + 7) `div` 8

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

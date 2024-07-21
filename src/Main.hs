module Main (main, twosum) where

import Data.Map (Map, (!?))
import Data.Maybe (listToMaybe, catMaybes)

import qualified Data.Map as M

-- | Return the rightmost pair of indices whose corresponding elements sum to `t`.

go :: Int -> (Int, Int) -> (Map Int Int, [Maybe (Int, Int)]) -> (Map Int Int, [Maybe (Int, Int)])
go target (i, x) (seen, sols) = (M.insert x i seen, sequence (i, seen !? (target-x)) : sols)

twosum :: Int -> [Int] -> Maybe (Int, Int)
twosum target = listToMaybe . catMaybes . snd . foldr (go target) (M.empty, []) . zip [0..]

main :: IO ()
main = do
  putStrLn "hello world"

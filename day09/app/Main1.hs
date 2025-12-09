module Main (main) where

import qualified Data.List.Split as LS (splitOn)

parse = map ((\(x:y:_) -> (x,y)) . map toInt . LS.splitOn ",") . lines
  where toInt = read :: String -> Int

solve [] = 0
solve ((x, y):ts) = maximum $ solve ts : map (calcArea (x, y)) ts
  where calcArea (x, y) (x2, y2) = (abs (x2 - x) + 1) * (abs (y2 - y) + 1)


main = getContents >>= print . solve . parse

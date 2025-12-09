module Main (main) where

import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L ((\\), any)

parse :: String -> [(Int, Int)]
parse = map ((\(x:y:_) -> (x,y)) . map toInt . LS.splitOn ",") . lines
  where toInt = read :: String -> Int

solve its = solve' its
  where solve' [] = 0
        solve' ((x, y):ts) = maximum $ solve' ts : map (calcArea (x, y)) (filter (\t ->  all ($ t) [isRectNotCrossed (x, y), isInArea (x, y)]) ts)
          where calcArea a@(x, y) b@(x2, y2) = (abs (x2 - x) + 1) * (abs (y2 - y) + 1)
                isInArea (x, y) (x2, y2) = let
                        topLeft = (min x x2, min y y2)
                        topRight = (max x x2, min y y2)
                        bottomLeft = (min x x2, max y y2)
                        bottomRight = (max x x2, max y y2)
                      in any (\(px, py) -> px <= fst topLeft && py <= snd topLeft) its
                        && any (\(px, py) -> px >= fst topRight && py <= snd topRight) its
                        && any (\(px, py) -> px <= fst bottomLeft && py >= snd bottomLeft) its
                        && any (\(px, py) -> px >= fst bottomRight && py >= snd bottomRight) its
                isRectNotCrossed a@(x, y) b@(x2, y2) = let
                         minX = min x x2
                         maxX = max x x2
                         minY = min y y2
                         maxY = max y y2
                         isLineCrossRect c@(px, py) d@(px2, py2) = (not ((a == c && b == d) || (a == d && b == c))
                          && (
                            (px > minX && px < maxX && py > minY && py < maxY)
                            || (px2 > minX && px2 < maxX && py2 > minY && py2 < maxY)
                            || (minX >= min px px2 && maxX <= max px px2 && py > minY && py < maxY && py2 > minY && py2 < maxY)
                            || (minY >= min py py2 && maxY <= max py py2 && px > minX && px < maxX && px2 > minX && px2 < maxX))
                          )
                       in not $ any (uncurry isLineCrossRect) $ zip its (tail its ++ [head its])

main = getContents >>= print . solve . parse

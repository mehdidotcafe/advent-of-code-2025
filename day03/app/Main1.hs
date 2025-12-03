module Main (main) where
import Data.Char (digitToInt)

parse = map (map digitToInt) . lines

solve bbs = let
  solve' bs = let
    m = maximum $ init bs
    in (read :: String -> Int) $ show m ++ show (maximum $ tail $ dropWhile (/= m) bs)
  in sum $ map solve' bbs

main = getContents >>= print . solve . parse

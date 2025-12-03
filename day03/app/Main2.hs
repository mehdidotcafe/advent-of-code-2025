module Main (main) where
import Data.Char (digitToInt)

parse = map (map digitToInt) . lines

solve bbs = let
  solve' i bs = case i of
    0 -> ""
    _ -> let
      m = maximum $ take (length bs - i + 1) bs
      in show m ++ solve' (i - 1) (tail $ dropWhile (/= m) bs)
  in sum $ map ((read :: String -> Int) . solve' 12) bbs

main = getContents >>= print . solve . parse

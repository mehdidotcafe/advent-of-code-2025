module Main (main) where 

import qualified Data.List.Split as LS (splitOn)
import Debug.Trace (trace)


parse = map ( (\(a:b:xs) -> (a, b)) . map (read :: String -> Int) . LS.splitOn "-") . LS.splitOn ","

isInvalidId pid = let
   isInvalidId' left right = case right of
     "" -> False
     (r:rs) -> left == right || isInvalidId' (left ++ [r]) rs
   in uncurry isInvalidId' $ splitAt (length pid `div` 2) pid
 
solve limits = let
  solve' limit = sum $ filter (isInvalidId . show) [fst limit .. snd limit]
  in sum $ map solve' limits

main = getContents >>= print . solve . parse

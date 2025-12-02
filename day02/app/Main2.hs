module Main (main) where

import qualified Data.List.Split as LS (splitOn)

parse = map ( (\(a:b:xs) -> (a, b)) . map (read :: String -> Int) . LS.splitOn "-") . LS.splitOn ","

isInvalidId pid = let
    isInvalidId'' left right = case right of
      "" -> True
      (r:rs) -> let
          (right_left, right_right) = splitAt (length left) right
        in (left == right_left && isInvalidId'' left right_right)
    isInvalidId' left right = case right of
      "" -> False
      (r:rs) -> isInvalidId'' left right || isInvalidId' (left ++ [r]) rs
   in uncurry isInvalidId' $ splitAt 1 pid

solve limits = let
    solve' limit = sum $ filter (isInvalidId . show) [fst limit .. snd limit]
  in sum $ map solve' limits

main = getContents >>= print . solve . parse

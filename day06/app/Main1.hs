module Main (main) where

import qualified Data.List as List (transpose)

parse = map format . List.transpose . map words . lines
  where format l = (parseOp $ last l, map (read :: String -> Int) $ init l)
        parseOp o = case o of
          "*" -> (*)
          "+" -> (+)

solve = sum . map (\(op, n:ns) -> foldr op n ns)

main = getContents >>= print . solve . parse

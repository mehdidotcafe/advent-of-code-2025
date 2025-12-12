module Main (main) where

import qualified Data.List.Split as LS (splitOn)
import Data.Bits as B (setBit)
import GHC.Bits (Bits(xor))

parse = map ((\ (x:xs) -> (parseIndicator x, map parseSchematic $ init xs)) . words) . lines
  where parseSchematic = foldr (flip B.setBit . toInt) 0 . LS.splitOn "," . init . tail
        parseIndicator = foldr ((\b acc -> acc * 2 + b) . fromEnum . (== '#')) 0 . init . tail
        toInt = read :: String -> Int

solve = sum . map (uncurry (dfs [0]))
  where dfs currentDiagrams diagram buttons = if diagram `elem` currentDiagrams
            then 0
            else 1 + dfs (concatMap (\d -> map (xor d) buttons) currentDiagrams) diagram buttons

main = getContents >>= print . solve . parse

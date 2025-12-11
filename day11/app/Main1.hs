module Main (main) where

import qualified Data.Map as M (fromList, (!))
import qualified Data.Set as S (fromList, insert, empty, member)

parse = M.fromList . map ((\(x:xs) -> (init x, xs) ). words) . lines

solve graph = solve' S.empty "you"
  where solve' visited key
          | key == "out" = 1
          | key `S.member` visited = 0
          | otherwise = sum $ map (solve' (S.insert key visited)) (graph M.! key)
main = getContents >>= print . solve . parse

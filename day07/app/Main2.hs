module Main (main) where

import qualified Data.List as L (partition)
import qualified Data.Set as S (fromList, member, singleton, unions, size, empty)
import qualified Data.MemoTrie as MT (memo)

parse c = let
  matrix = lines c
  limits = (length matrix, length $ head $ matrix)
  formatTuple (start, splitters) = (keepCoords $ head start, S.fromList $ map keepCoords splitters)
  keepCoords (_, y, x) = (y, x)
  filterCoords = concatMap (filter (\(a,_,__) -> a == '^' || a == 'S') . (\(y, l) -> zip3 l (repeat y) [0..])) . zip [0..]
  in (formatTuple . L.partition (\(a, _, __) -> a == 'S') . filterCoords $ matrix, limits)

solve ipos splitters (ly, lx) = 1 + solve'' ipos
  where solve' pos@(y, x)
          | isOutOfBounds pos = 0
          | pos `S.member` splitters = 1 + (solve'' (y, x - 1) + solve'' (y, x + 1))
          | otherwise = solve'' (y + 1, x)
        solve'' = MT.memo solve'
        isOutOfBounds (y, x) = y < 0 || y > ly || x < 0 || x > lx

main = getContents >>= print . uncurry (uncurry solve) . parse

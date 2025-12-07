module Main (main) where

import qualified Data.List as L (partition)
import qualified Data.Set as S (fromList, member, singleton, unions, size, empty)
import qualified Data.MemoTrie as MT (memo)

parse c = let
  matrix = lines c
  limits = (length matrix, length $ head $ matrix)
  formatTuple (start, splitters) = (keepCoords $ head start, S.fromList $ map keepCoords splitters)
  keepCoords (_, y, x) = (y, x)
  in (formatTuple . L.partition (\(a, _, __) -> a == 'S') . concatMap (filter (\(a,_,__) -> a == '^' || a == 'S') . (\(y, l) -> zip3 l (repeat y) [0..])) . zip [0..] $ matrix, limits)

solve ipos splitters (ly, lx) = S.size $ solve'' ipos
  where solve' pos@(y, x)
          | isOutOfBounds pos = S.empty
          | pos `S.member` splitters = S.unions [S.singleton pos, solve'' (y, x - 1), solve'' (y, x + 1)]
          | otherwise = solve'' (y + 1, x)
        solve'' = MT.memo solve'
        isOutOfBounds (y, x) = y < 0 || y > ly || x < 0 || x > lx

main = getContents >>= print . uncurry (uncurry solve) . parse

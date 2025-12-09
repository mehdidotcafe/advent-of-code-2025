module Main (main) where

import qualified Data.List as L (sortBy, (\\), find)
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Set as S (member, fromList, toList, union, singleton, size)
import Data.Ord (Down(Down), comparing)

parse = map (map toInt . LS.splitOn ",") . lines
  where toInt = read :: String -> Int

solve cs = (\(a, b) -> head a * head b) $ getLargeCircuit $ sortClosestPairs cs
      where sortClosestPairs bs = map snd $ L.sortBy (comparing fst) $ map (\(a, b) -> (euclideanDistance a b, (a, b))) $ S.toList $ S.fromList $ concatMap (\b -> [(min b a, max b a) | a <- bs, a /= b]) bs
            euclideanDistance (x1:y1:z1:_) (x2:y2:z2:_) = sqrt $ fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)
            getLargeCircuit = makeCircuit' []
              where makeCircuit' acc (p:ps) = let
                      fc = getCircuit acc (fst p)
                      sc = getCircuit acc (snd p)
                      c = fc `S.union` sc
                      in if S.size c == length cs then p else makeCircuit' ((fc `S.union` sc) : (acc L.\\ [fc, sc])) ps
            getCircuit cs c = case L.find (S.member c) cs of
                Nothing -> S.singleton c
                Just v -> v

main = getContents >>= print . solve . parse

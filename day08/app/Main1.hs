module Main (main) where

import qualified Data.List as L (sortBy, (\\), find)
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Set as S (member, fromList, toList, union, singleton)
import Data.Ord (Down(Down), comparing)

parse = map (map toInt . LS.splitOn ",") . lines
  where toInt = read :: String -> Int

solve np m = product . take m . L.sortBy (comparing Down) . map length . makeCircuits . take np . sortClosestPairs
      where sortClosestPairs bs = map snd $ L.sortBy (comparing fst) $ map (\(a, b) -> (euclideanDistance a b, (a, b))) $ S.toList $ S.fromList $ concatMap (\b -> [(min b a, max b a) | a <- bs, a /= b]) bs
            euclideanDistance (x1:y1:z1:_) (x2:y2:z2:_) = sqrt $ fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)
            getCircuit cs c = case L.find (S.member c) cs of
                Nothing -> S.singleton c
                Just v -> v
            makeCircuits = foldr (\ (a, b) cs -> let
                ac = getCircuit cs a
                bc = getCircuit cs b
              in (ac `S.union` bc) : (cs L.\\ [ac, bc]) ) []
main = getContents >>= print . solve 1000 3 . parse

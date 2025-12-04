module Main (main) where

import qualified Data.Set as Set (fromList, empty, intersection, union, singleton, null, (\\))

parse = Set.fromList . concatMap (map (\(a,b,c) -> (b, c)) . filter (\(a,_,__) -> a == '@') . (\(y, l) -> zip3 l (repeat y) [0..])) . zip [0..] . lines

solve irs = let
   neighbors y x = [(y+dy, x+dx) | dy <- [-1,0,1], dx <- [-1,0,1], (dy,dx) /= (0,0)]
   solve' rs = case foldr (solve'' rs) Set.empty rs of
      s | Set.null s -> 0
        | otherwise -> length s + solve' (rs Set.\\ s)
   solve'' rs r@(y,x) v
     | length (rs `Set.intersection` Set.fromList (neighbors y x)) >= 4 = v
     | otherwise = v `Set.union` Set.singleton r
   in solve' irs
main = getContents >>= print . solve . parse

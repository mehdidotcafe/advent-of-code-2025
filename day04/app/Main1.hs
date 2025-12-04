module Main (main) where

import qualified Data.Set as Set (fromList, map, empty, intersection, union, singleton)

parse = Set.fromList . concatMap (map (\(a,b,c) -> (b, c)) . filter (\(a,_,__) -> a == '@') . (\(y, l) -> zip3 l (repeat y) [0..])) . zip [0..] . lines

solve rs = let
  neighbors y x = [(y+dy, x+dx) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
  solve' r@(y,x) v
    | length (rs `Set.intersection` Set.fromList (neighbors y x)) >= 4 = v
    | otherwise = v `Set.union` Set.singleton r
  in length $ foldr solve' Set.empty rs

main = getContents >>= print . solve . parse

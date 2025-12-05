module Main (main) where

import qualified Data.List.Split as LS (splitOn)
import qualified Data.Set as Set (foldr, empty, union, singleton, delete, insert, filter, (\\), null, toList, map)

parse = range . takeWhile (/= "") . lines
  where toInt = read :: String -> Int
        range = map parseRange
          where parseRange r = (\(ds, de) -> (toInt ds, toInt $ tail de)) $ break (== '-') r

solve = count . foldr combineRanges Set.empty
  where combineRanges r acc = case isOverlapping r acc of
          dvs | Set.null dvs -> acc `Set.union` Set.singleton r
            | otherwise -> Set.insert (computeRange dvs r) $ acc Set.\\ dvs
        isOverlapping (rs, re) = Set.filter hasOverlap
          where hasOverlap (as, ae) = (rs >= as && rs <= ae) || (re >= as && re <= ae) || (as >= rs && as <= re) || (ae >= rs && ae <= re)
        computeRange dvs (rs, re) = (minimum $ rs:Set.toList (Set.map fst dvs), maximum $ re:Set.toList (Set.map snd dvs))
        count = Set.foldr (\(s,e) acc -> acc + e - s + 1) 0

main = getContents >>= print . solve  . parse

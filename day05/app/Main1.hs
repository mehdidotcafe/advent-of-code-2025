module Main (main) where

import qualified Data.List.Split as LS (splitOn)
import qualified Data.Set as Set (fromList, intersection, foldr, filter, null)

parse = (\(rs, ids) -> (range rs, map toInt $ tail ids)) . break (== "") . lines
  where toInt = read :: String -> Int
        range = Set.fromList . map parseRange
          where parseRange r = (\(ds, de) -> (toInt ds, toInt $ tail de)) $ break (== '-') r

solve rs ids = length $ filter (\id -> not $ Set.null $ Set.filter(\(rs, re) -> rs <= id && re >= id) rs) ids

main = getContents >>= print . uncurry solve  . parse

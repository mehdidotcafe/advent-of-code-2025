module Main (main) where

import Data.List.Split qualified as LS (splitOn)
import Data.Map.Strict qualified as M (fromList, (!))
import Data.Set qualified as S (fromList, map, size)

parse c =
  let rows = LS.splitOn [""] $ lines c
      presents = M.fromList $ map (\(x : xs) -> (toInt $ init x, S.fromList $ concatMap (\(y, ps) -> map (\x -> (fst x, y)) ps) $ zip [0 ..] (map (zip [0 ..]) xs))) $ init rows
      regions = map ((\(s : p : _) -> (parseSizes s, parsePresents p)) . LS.splitOn ": ") (last rows)
      parseSizes = (\(x : y : _) -> (x, y)) . map toInt . LS.splitOn "x"
      parsePresents = concatMap (\(i, c) -> replicate c i) . zip [0 ..] . map toInt . words
      toInt = read :: String -> Int
   in (presents, regions)

solve presents = length . filter (uncurry solve')
    where solve' (w, h) pis = w * h >= sum (map (S.size . (presents M.!)) pis)

main = getContents >>= print . uncurry solve . parse

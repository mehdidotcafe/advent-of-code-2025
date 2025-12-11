module Main (main) where

import qualified Data.Map as M (fromList, findWithDefault)
import qualified Data.MemoTrie as MT (memo2)

parse = M.fromList . map ((\(x:xs) -> (init x, xs) ). words) . lines

solve graph = (\(a,b) -> product a + product b) $ splitAt 3 $ map (uncurry getPaths') [
    ("svr", "fft"),
    ("fft", "dac"),
    ("dac", "out"),
    ("svr", "dac"),
    ("dac", "fft"),
    ("fft", "out")
  ]
  where
        getPaths node target = if node == target
                then 1
                else sum $ map (`getPaths'` target) $ M.findWithDefault [] node graph
        getPaths' = MT.memo2 getPaths

main = getContents >>= print . solve . parse

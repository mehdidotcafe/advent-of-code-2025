module Main (main) where

import qualified Data.List as L (transpose)
import qualified Data.List.Split as LS (splitWhen)

parse = map (\ns -> (parseOp $ last $ head ns, map (toInt . init) ns)) . LS.splitWhen  (all (== ' ')) . L.transpose .  lines
  where parseOp o = case o of
          '*' -> (*)
          '+' -> (+)
        toInt = read :: String -> Int

solve =  sum . map (\(op, n:ns) -> foldr op n ns)

main = getContents >>= print . solve . parse

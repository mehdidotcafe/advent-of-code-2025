module Main (main) where

getSign c = case c of
                'L' -> -1
                'R' -> 1

parse = map (\(x:xs) -> getSign x * (read :: String -> Int) xs) . lines

hasMadeClick r pos dc
  | r > 0 = (r + pos) `div` dc
  | otherwise = (abs r + ((dc - pos) `mod` dc)) `div` dc

solve pos dc rotations = case rotations of
  [] -> 0
  (r:rs) -> let
    start' = (pos + r) `mod` dc
    in hasMadeClick r pos dc + solve start' dc rs

main = getContents >>= print . solve 50 100 . parse
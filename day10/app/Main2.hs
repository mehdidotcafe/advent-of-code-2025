module Main (main) where

import qualified Data.List as L (foldl')
import qualified Data.List.Split as LS (splitOn)
import Control.Monad (forM, forM_)
import Z3.Monad

parse = map (parseLine . words) . lines
  where
    parseLine (x : xs) =
      let levels = map toInt . LS.splitOn "," . init $ tail $ last xs
          schematics = map (map toInt . LS.splitOn "," . init . tail) (init xs)
          toInt = read :: String -> Int
       in (map toInteger levels, schematics)

solve target buttons = evalZ3 $ do
  let n = length target
      m = length buttons
      bMatrix = buildMatrix n m buttons

  xs <- forM [0 .. m - 1] $ \j ->
    mkFreshIntVar ("x_" ++ show j)

  zero <- mkInteger 0

  forM_ xs $ \xj -> do
    ge0 <- mkGe xj zero
    optimizeAssert ge0

  forM_ (zip target bMatrix) $ \(tiVal, rowCoeffs) -> do
    lhs <- linearCombination rowCoeffs xs zero
    ti <- mkInteger tiVal
    eq <- mkEq lhs ti
    optimizeAssert eq

  totalPresses <- sumTerms xs zero
  _ <- optimizeMinimize totalPresses

  res <- optimizeCheck []
  case res of
    Sat -> do
      model <- optimizeGetModel
      forM xs $ \xj -> do
        mv <- evalInt model xj
        case mv of
          Just v -> return v
          Nothing -> fail "Error"
    _ -> return []

sumTerms [] zero = return zero
sumTerms [x] _ = return x
sumTerms (x : xs) _ = mkAdd (x : xs)

linearCombination coeffs vars zero = do
  let pairs = [(c, v) | (c, v) <- zip coeffs vars, c /= 0]
  case pairs of
    [] -> return zero
    [(c, v)] -> scaleTerm c v
    _ -> do
      ts <- mapM (uncurry scaleTerm) pairs
      mkAdd ts
  where
    scaleTerm 1 v = return v
    scaleTerm c v = do
      cAst <- mkInteger c
      mkMul [cAst, v]

buildMatrix n m buttons =
  L.foldl' addButton base (zip [0 ..] buttons)
  where
    base = replicate n (replicate m 0)
    addButton b (j, btn) = foldl' (\b' i -> incCoeff i j b') b btn
    incCoeff i j = modifyAt i (modifyAt j (+ 1))

modifyAt k f xs =
  let (before, x : after) = splitAt k xs
   in before ++ f x : after

main = getContents >>= mapM (uncurry solve) . parse >>= print . sum . concat

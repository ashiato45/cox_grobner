module Groebner where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Distribution.Simple.Utils
import MultiDeg
import MultiPoly
import Debug.Trace

-- import qualified Data.Map as DMap

calcSPoly :: (MultiDeg multideg, Ord coef, Fractional coef, Show multideg, Show coef)
  => Poly coef multideg -> Poly coef multideg -> Poly coef multideg
calcSPoly f g =
  let
    l = lcmPoly f g
    n = getPolyNumber f
    df = l `divide` [Poly n $ [getLT(f)]]
    dg = l `divide` [Poly n $ [getLT(g)]]
    qf = snd.(!! 0).divisors $ df
    qg = snd.(!! 0).divisors $ dg
  in
    normalize $ qf * f - qg * g

makePairs :: [a] -> [(a,a)]
makePairs [] = []
makePairs [_] = []
makePairs (x:xs) = ((\y -> (x,y)) <$> xs) ++ makePairs xs

data SPairCheckLog coef multideg =
  SPCLogDivision {
    spcP1 :: Poly coef multideg,
    spcP2 :: Poly coef multideg,
    spcDivisor :: [Poly coef multideg],
    spcPoly :: Poly coef multideg,
    spcRemainder :: Poly coef multideg,
    spcDivisionLog :: [DivisionLog coef multideg]
    }

sPairCheck' :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  Poly coef multideg -> Poly coef multideg -> [Poly coef multideg] ->
  Writer [SPairCheckLog coef multideg] Bool
sPairCheck' p1_ p2_ ds = do
  let s = calcSPoly p1_ p2_
  let DivisionState _ _ _ r log_ = divide s ds
  let w = SPCLogDivision p1_ p2_ ds s r log_
  tell [w]
  return $ getMultideg r == moEmpty


sPairCheck :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  [Poly coef multideg] -> Writer [SPairCheckLog coef multideg] Bool
sPairCheck xs = let
    s = [(a,b) | (a,b) <- makePairs xs]
    s2 = [sPairCheck' a b xs | (a,b) <- s]
  in
    do
      rs <- sequence s2
      return $ or rs

data CGroebnerLog coef multideg =
  CGLogStart{
    cglInit :: [Poly coef multideg]
  }
  |CGLogCalcSPoly{
    cglP1 :: Poly coef multideg,
    cglP2 :: Poly coef multideg,
    cglRemainder :: Poly coef multideg
  }
  |CGLogAppend{
    cglAppend :: [Poly coef multideg]
  }
  |CGLogCompleted{
    cglCompleted :: [Poly coef multideg]
  }

calcSPolyLog :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  (Poly coef multideg) -> (Poly coef multideg) -> [Poly coef multideg] -> Writer [CGroebnerLog coef multideg] (Poly coef multideg)
calcSPolyLog x y ds = do
            let s = calcRemainder (calcSPoly x y)  ds
            tell [CGLogCalcSPoly x y s]
            return s

calcGroebner' :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  [Poly coef multideg] -> [Poly coef multideg] ->
  Writer [CGroebnerLog coef multideg] [Poly coef multideg]
calcGroebner' xs ys = do
  let f x y = calcSPolyLog x y (xs++ys)
  oldNew <- sequence $ [f x y  | x <- xs, y <- ys]
  newNew <- sequence $ [f a b | (a, b) <- makePairs ys]
  let nonzeroOldNew = nub oldNew
  let nonzeroNewNew = nub newNew
  let a = filter (\x -> moEmpty /= getMultideg x) $ nub $ nonzeroOldNew ++ nonzeroNewNew
  let next = xs ++ ys
  if a == []
  then do
          tell [CGLogCompleted {cglCompleted = next}]
          return next
  else do
          tell [CGLogAppend [head a]]
          calcGroebner' next [head a]

calcGroebner :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  [Poly coef multideg] -> Writer [CGroebnerLog coef multideg] [Poly coef multideg]
calcGroebner xs = do
  tell $ [CGLogStart xs]
  calcGroebner' [] xs

data MGroebnerLog coef multideg =
  MGLogStart{
    mglInit :: [Poly coef multideg]
  }
  |MGLogRemove{
    mglRemoved :: Poly coef multideg,
    mglRemoving :: Poly coef multideg}
  |MGLogCompleted{
    mglCompleted :: [Poly coef multideg]
  }

minimalizeGroebner' :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  [Poly coef multideg] -> [Poly coef multideg] -> Writer [MGroebnerLog coef multideg] [Poly coef multideg]
minimalizeGroebner' [] ys = do
  let mys = map monicalize ys
  tell [MGLogCompleted {mglCompleted = mys}]
  return mys
minimalizeGroebner' (x:xs) ys = do
  let pr p2 = (getMultideg x) `moGEAll` (getMultideg p2)
  let fn = find pr (delete x ys)
  case fn of
    Nothing -> minimalizeGroebner' xs ys
    Just p -> do
      tell [MGLogRemove{
        mglRemoved = x,
        mglRemoving = p
      }]
      minimalizeGroebner' xs (delete x ys)

minimalizeGroebner :: (MultiDeg multideg, Fractional coef, Show coef, Ord coef) =>
  [Poly coef multideg] -> Writer [MGroebnerLog coef multideg] [Poly coef multideg]
minimalizeGroebner xs = do
  tell [MGLogStart xs]
  minimalizeGroebner' xs xs

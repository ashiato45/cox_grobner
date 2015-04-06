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

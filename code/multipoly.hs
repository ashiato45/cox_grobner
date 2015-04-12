module MultiPoly where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Distribution.Simple.Utils
import MultiDeg
-- import qualified Data.Map as DMap

data Term coef multideg = Term {coef :: coef, multideg :: multideg} deriving Show
data Poly coef multideg = Poly Int [Term coef multideg] deriving Show


zeroTerm :: (Fractional coef, MultiDeg multideg) => Term coef multideg
zeroTerm = Term {coef = 0, multideg = moEmpty}

constTerm :: (Fractional coef, MultiDeg multideg) => coef -> Term coef multideg
constTerm c = Term {coef = c, multideg = moZero}

alignTerm :: (Fractional coef, MultiDeg multideg) => Int -> Term coef multideg -> Term coef multideg
alignTerm n (Term{coef=coef, multideg=multideg}) = Term{coef=coef, multideg=moAlign n multideg}

instance (Fractional coef, MultiDeg multideg) => Num (Term coef multideg) where
    t1 + t2 = if multideg t1 == moEmpty || multideg t2 == moEmpty
              then if multideg t1 == moEmpty
                   then t2
                   else t1
              else if multideg t1 == multideg t2
                   then Term{coef=coef t1 + coef t2, multideg = multideg t1}
                   else error "Invalid monomials addition"
    negate t1 = Term{coef=negate $ coef t1, multideg = multideg t1}
    t1 * t2 = Term {coef=coef t1 * coef t2, multideg = multideg t1 `moPlus` multideg t2}
    abs t1 = Term {coef=abs $ coef t1, multideg = multideg t1}
    signum t1 = Term {coef=signum $ coef t1, multideg = multideg t1}
    fromInteger n = Term{coef = 0, multideg = moEmpty}

instance (Ord coef, Fractional coef, MultiDeg multideg) => Eq (Term coef multideg) where
    x == y = (coef x) == (coef y) && (multideg x) == (multideg y)

instance (Ord coef, Fractional coef, MultiDeg multideg) => Ord (Term coef multideg) where
    compare x y = if multideg x == multideg y
                  then compare (coef x) (coef y)
                  else compare (multideg x) (multideg y)


alignTerms :: (Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg
alignTerms (Poly n xs) = Poly n $ map (alignTerm n) xs

reduceZeroTerms :: (Eq coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg
reduceZeroTerms (Poly n xs) = Poly n $ filter ((/= 0).coef) $ filter ((/= moEmpty).multideg) xs

simplify :: (Eq coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg
simplify (Poly n xs) = reduceZeroTerms $ (Poly n) $ map (foldr (+) zeroTerm) $ groupBy (equating multideg) xs

sortTerms :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg
sortTerms (Poly n xs) = Poly n $ (reverse.sort) xs

normalize :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg
normalize = sortTerms.simplify.alignTerms

instance (Eq coef, Ord coef, Fractional coef, MultiDeg multideg) => Num (Poly coef multideg) where
    (Poly n xs) + (Poly m ys) = normalize $ Poly (max n m) $ xs ++ ys
    negate (Poly n xs) = Poly n $ map negate xs
    (Poly n xs) * (Poly m ys) = normalize $ Poly (max n m) $ (*) <$> xs <*> ys
    abs = id
    signum (Poly n xs) = normalize $ Poly n $ [Term{coef=1, multideg=moZero}]
    fromInteger n = undefined

instance (Eq coef, Ord coef, Fractional coef, MultiDeg multideg) => Eq (Poly coef multideg) where
    (Poly n xs) == (Poly m ys) = n == m && xs == ys

negativeParen :: Rational -> String -> String
negativeParen r s = if r < 0
                    then "(" ++ s ++ ")"
                    else s

chooseChars :: Int -> [String]
chooseChars n = let chars = ["x", "y", "z", "w"]
                    numbered = ["x_" ++ (show u) | u <- [0..]]
                in if n > length chars
                   then numbered
                   else chars

getMultidegList :: PlainMonomial -> [Int]
getMultidegList EmptyMonomial = []
getMultidegList (PlainMonomial xs) = xs

getLT :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Term coef multideg
getLT p = let Poly n xs = normalize p
          in if xs == []
             then zeroTerm
             else head xs

getMultideg :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> multideg
getMultideg p = multideg $ getLT p

getLM :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Term coef multideg
getLM p = let m = getMultideg p
          in if m == moEmpty
             then zeroTerm
             else Term{coef=1, multideg=m}

getLC :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> coef
getLC p = if getMultideg p == moEmpty
          then 0
          else coef $ getLT p

getMonoPoly ::  (Ord coef, Fractional coef, MultiDeg multideg) => Int -> Term coef multideg -> Poly coef multideg
getMonoPoly n t = normalize $ Poly n [t]

data DivisionLog coef multideg = DLogMove (Poly coef multideg)
                               | DLogDivides (Poly coef multideg) (Poly coef multideg)
                               | DLogCompleted [(Poly coef multideg)] (Poly coef multideg)
                               | DLogStart (Poly coef multideg) [(Poly coef multideg)]


data DivisionState coef multideg = DivisionState {dividend  :: Poly coef multideg,
                                                  divisors  :: [((Poly coef multideg),(Poly coef multideg))],
                                                  stock     :: Poly coef multideg,
                                                  remainder :: Poly coef multideg,
                                                  divLog    :: [DivisionLog coef multideg]}

polyDivides :: (Ord coef, Fractional coef, MultiDeg multideg) => Poly coef multideg -> Poly coef multideg -> Bool
polyDivides p1 p2 = let l1 = getMultideg p1
                        l2 = getMultideg p2
                    in if l1 == moEmpty
                       then False
                       else if l2 == moEmpty
                            then True
                            else let PlainMonomial i1 = moGetPlain l1
                                     PlainMonomial i2 = moGetPlain l2
                                 in and $ zipWith (<=) i1 i2


tellDivLog :: (Ord coef, Fractional coef, MultiDeg multideg) => (DivisionLog coef multideg) -> State (DivisionState coef multideg) ()
tellDivLog s = do
  DivisionState{dividend=dividend,
                divisors=divisors,
                stock=stock,
                remainder=remainder,
                divLog=divLog} <- get
  put DivisionState{dividend=dividend,
                    divisors=divisors,
                    stock=stock,
                    remainder=remainder,
                    divLog=s:divLog}
  return ()

replace :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
replace k v xs = let (h, t) = splitAt (fromJust $ findIndex (((== k).fst)) xs) xs
                 in (h) ++ [(k, v)] ++ tail t

divide' :: (Ord coef, Fractional coef, Show coef, Show multideg, MultiDeg multideg) => State (DivisionState coef multideg) ()
divide' = do
  DivisionState{dividend=dividend,
                divisors=divisors,
                stock=stock,
                remainder=remainder,
                divLog=divLog} <- get
  if getMultideg stock == moEmpty
  then do
    tellDivLog $ DLogCompleted (map snd divisors) remainder
    return ()
  else do
    let found = find (`polyDivides` stock) $ (map fst) divisors
    let Poly n _ = dividend
    let lp = getMonoPoly n $ getLT stock
    if isNothing found
    then do
      put DivisionState{
                dividend=dividend,
                divisors=divisors,
                stock=normalize $ stock - lp,
                remainder=normalize $ remainder + lp,
                divLog=divLog}
      tellDivLog $ DLogMove lp
      divide'
    else do
      let d = fromJust found
      let scaleDeg = (getMultideg stock) `moMinus` (getMultideg  d)
      let scale = getMonoPoly n $ Term{coef=(getLC stock)/(getLC d), multideg=scaleDeg}
      let newStock = normalize $ stock - scale*d
      let newQuot = normalize $(fromJust (d `lookup` divisors)) + scale
      put DivisionState{
                dividend=dividend,
                divisors=replace d newQuot divisors,
                stock=newStock,
                remainder=remainder,
                divLog=divLog
              }
      tellDivLog $ DLogDivides d newStock
      divide'

getPolyNumber ::  (Ord coef, Fractional coef, Show coef, Show multideg, MultiDeg multideg) => (Poly coef multideg) -> Int
getPolyNumber (Poly n _) = n

divide :: (Ord coef, Fractional coef, Show coef, Show multideg, MultiDeg multideg) => (Poly coef multideg) -> [Poly coef multideg] -> (DivisionState coef multideg)
divide p1 p2s = snd $ runState  divide' (DivisionState{dividend=p1,
                                                       divisors=map (\x -> (x,Poly (getPolyNumber p1) [])) p2s,
                                                       stock=p1,
                                                       remainder=Poly (getPolyNumber p1) [],
                                                       divLog=[DLogStart p1 p2s]})


calcRemainder :: (Ord coef, Fractional coef, Show coef, Show multideg, MultiDeg multideg) =>
  (Poly coef multideg) -> [Poly coef multideg] -> (Poly coef multideg)
calcRemainder p1 p2s =
  let
    (DivisionState{dividend=_, divisors=_, stock=_, remainder=r_, divLog=_}) = divide p1 p2s
  in
    r_


makeLexPoly' :: Int -> (Rational, [Int]) -> Term Rational Lex
makeLexPoly' n (r, xs) = Term r (Lex (PlainMonomial xs))

makeLexPoly :: Int -> [(Rational, [Int])] -> Poly Rational Lex
makeLexPoly n xs = Poly n $ map (makeLexPoly' n) xs

makePoly' :: (MultiDeg multideg) => Int -> (Rational, [Int]) -> Term Rational multideg
makePoly' n (r, xs) = Term r (moMake (PlainMonomial xs))

makePoly :: (MultiDeg multideg) => Int -> [(Rational, [Int])] -> Poly Rational multideg
makePoly n xs = normalize $ Poly n $ map (makePoly' n) xs

lcmPoly :: (MultiDeg multideg, Fractional coef, Ord coef) => Poly coef multideg -> Poly coef multideg -> Poly coef multideg
lcmPoly p q = let
                (Poly n _) = p
                mp = getMultideg p
                mq = getMultideg q
              in
                Poly n [Term{coef = 1, multideg = mp `moMax` mq}]

monicalize :: (MultiDeg multideg, Fractional coef, Ord coef) => (Poly coef multideg) -> Poly coef multideg
monicalize p@(Poly n_ terms_) =
  let
    f (Term c_ m_) = Term (c_ / getLC p) (m_)
  in
    Poly n_ (map f terms_)

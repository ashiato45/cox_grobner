import Data.Monoid
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ratio

data Term a = Term{deg :: Int, coef :: a} deriving (Show, Eq)
data Polynomial a = Polynomial{terms :: [Term a]} deriving Show
data DivResult a = DivResult {q :: Polynomial a, r :: Polynomial a} deriving Show

instance (Ord a) => Ord (Term a) where
    x > y = if deg x == deg y then
                coef x > coef y
            else
                deg x > deg y
    x < y = (not (x > y)) || (x == y)
    x >= y = x > y || x == y
    x <= y = x < y || x == y

classify :: (a -> a-> Bool) -> [a] -> [[a]]
classify _ [] = []
classify f all@(x:xs) = let (yes, no) = partition (f x) all
                      in yes:(classify f no)

addTermClass :: (Ord a, Num a) => [Term a] -> Term a
addTermClass [] = Term{deg = 0, coef = 0}
addTermClass all@(x:xs) = Term{deg = deg x, coef = sum (coef <$> all)}

emptyToZero :: (Ord a, Num a) => [Term a] -> [Term a]
emptyToZero [] = [Term{deg = (-1), coef = 0}]
emptyToZero xs = xs

normalize :: (Ord a, Num a) => Polynomial a -> Polynomial a
normalize (Polynomial{terms=ts}) = let c = classify (\x y -> deg x == deg y) ts
                                       f Term{deg=d, coef=c} = (c /= 0)
                                  in Polynomial{terms = emptyToZero $ filter f (addTermClass <$> c)}

genPoly :: (Num a) => [a] -> Polynomial a
genPoly x = let f (x,y) = Term{deg = x, coef = y}
            in Polynomial{terms = f <$> (zip [0..] x)}

zeroPoly :: (Num a,Ord a) => Polynomial a
zeroPoly = normalize Polynomial{terms = []}

showTerm :: (Show a, Num a) => Term a -> String
showTerm Term{deg = d, coef = c} = (show c) ++ "x^" ++ (show d)

showPolynomial :: (Show a, Num a) => Polynomial a -> String
showPolynomial Polynomial{terms=ts} = intercalate "+" (showTerm <$> ts)

getDegree :: (Num a) => Polynomial a -> Int
getDegree Polynomial{terms = []} = -1
getDegree Polynomial{terms = ts} = foldr1 max (deg <$> ts)

getLT :: (Ord a,Num a) => Polynomial a -> Term a
getLT Polynomial{terms=ts} = foldr max (Term{deg = -1, coef = 0}) ts

addPolynomial :: (Ord a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
addPolynomial Polynomial{terms=f} Polynomial{terms=g} = normalize Polynomial{terms = f++g}

scalePolynomial :: (Ord a, Num a) => a -> Polynomial a -> Polynomial a
scalePolynomial s Polynomial{terms=ts} = normalize Polynomial{terms = (f s) <$> ts}
    where f s Term{deg=d, coef=c} = Term{deg=d, coef=s*c}

multiplyPolynomial :: (Ord a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPolynomial Polynomial{terms=fs} Polynomial{terms=gs} = normalize Polynomial{terms = f <$> fs <*> gs}
    where f x y = Term{deg=deg x + deg y, coef = coef x * coef y}

subtractPolynomial :: (Ord a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
subtractPolynomial f g = addPolynomial f (scalePolynomial (-1) g)

embedTerm :: (Num a) => Term a -> Polynomial a
embedTerm x  = Polynomial{terms=[x]}

-- When Polynomial{terms = []} given?
divide :: (Ord a,Fractional a) => Polynomial a -> Polynomial a -> Maybe (DivResult a)
divide f g = if getDegree g < 0 then
                 Nothing
             else
                 let flt = getLT f
                     glt = getLT g
                     scale = Term{deg = deg flt - deg glt, coef = coef flt / coef glt}
                     escale = embedTerm scale
                 in if (deg scale < 0) then
                        Just $ DivResult{q = zeroPoly, r = normalize $ f}
                    else
                        let sub = multiplyPolynomial escale g
                            tempR = normalize $ subtractPolynomial f sub
                            under = fromJust $ divide tempR g
                        in Just $ DivResult{q = normalize $ addPolynomial escale (q under), r = normalize $ r under}

showDivResult :: (Show a, Ord a, Fractional a) => DivResult a -> String
showDivResult DivResult{q = q, r = r} = "Quotient:" ++ (showPolynomial q) ++ ", Remaining:" ++ (showPolynomial r)
                     

main = do
  let p = Polynomial{terms=[Term{deg=0, coef=1}, Term{deg=0, coef=2}, Term{deg=1,coef=3}]}
  putStrLn $ show p
  putStrLn $ show $ classify (\x y -> deg x == deg y) (terms p)
  putStrLn $ show $ normalize p
  let q = genPoly([0,4,8])
  putStrLn $ showPolynomial $ normalize p
  putStrLn $ showPolynomial q
  putStrLn $ show $ getDegree q
  putStrLn $ show $ getLT q
  putStrLn $ showPolynomial $ addPolynomial p q
  putStrLn $ showPolynomial $ scalePolynomial 2 q
  putStrLn $ showPolynomial $ multiplyPolynomial p q
  putStrLn $ showPolynomial $ embedTerm Term{deg=2, coef=100}
  putStrLn $ showDivResult $ fromJust $ divide (genPoly ([1,2,1] :: [Rational])) (genPoly ([1,1] :: [Rational]))
  -- let p = Polynomial {terms = [0,1]}
  -- let f = Polynomial{terms=[1,1]}
  -- let g = Polynomial{terms=[2,2,2]}
  -- putStrLn $ show f
  -- putStrLn $ show g
  -- putStrLn $ show $ f > g


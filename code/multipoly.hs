module MultiPoly where

import Control.Applicative
import Data.List
import Distribution.Simple.Utils
import Data.Ratio
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as DMap

data PlainMonomial = PlainMonomial [Int]|EmptyMonomial deriving Show
data Term coef multideg = Term {coef :: coef, multideg :: multideg} deriving Show
data Poly coef multideg = Poly Int [Term coef multideg] deriving Show
newtype Lex = Lex PlainMonomial deriving Show
newtype GrLex = GrLex PlainMonomial deriving Show

class (Ord a) => MultiDeg a where
    moPlus :: a -> a -> a
    moNeg :: a -> a
    moMinus :: a -> a -> a
    moMinus x y = moPlus x $ moNeg y
    moEmpty :: a
    moZero :: a
    moTotalOrder :: a -> Maybe Int
    moAlign :: Int -> a -> a
    moMake :: PlainMonomial -> a
    moGetPlain :: a -> PlainMonomial
    moShow :: a -> String

instance Eq PlainMonomial where
    EmptyMonomial == EmptyMonomial = True
    EmptyMonomial == PlainMonomial _ = False
    PlainMonomial _ == EmptyMonomial = False
    PlainMonomial xs == PlainMonomial ys = xs == ys

instance Eq Lex where
    (Lex x) == (Lex y) = x == y

instance Ord Lex where
    compare (Lex (PlainMonomial [])) _ = error "Invalid PlainMonomial"
    compare _ (Lex (PlainMonomial [])) = error "Invalid PlainMonomial"
    compare (Lex EmptyMonomial) (Lex EmptyMonomial) = EQ
    compare (Lex EmptyMonomial) _ = LT
    compare _ (Lex EmptyMonomial) = GT
    compare (Lex (PlainMonomial [x])) (Lex (PlainMonomial [y])) = compare x y
    compare (Lex (PlainMonomial (x:xs))) (Lex (PlainMonomial (y:ys))) = if x == y
                                        then compare xs ys
                                        else compare x y

instance MultiDeg Lex where
    moPlus _ (Lex EmptyMonomial) = Lex EmptyMonomial
    moPlus (Lex EmptyMonomial) _ = Lex EmptyMonomial
    -- moPlus (Lex (PlainMonomial xs)) (Lex (PlainMonomial ys)) = Lex $ PlainMonomial $ (+) <$> xs <*> ys
    moPlus (Lex (PlainMonomial xs)) (Lex (PlainMonomial ys)) = Lex $ PlainMonomial $ zipWith (+) (xs ++ (repeat 0)) (ys ++ (repeat 0))
    moNeg (Lex EmptyMonomial) = error "No reciprocal of EmptyMonomial! It's just the zero division!!"
    moNeg (Lex (PlainMonomial xs)) = Lex $ PlainMonomial $ negate <$> xs
    moEmpty = Lex EmptyMonomial
    moZero = Lex $ PlainMonomial $ repeat 0
    moTotalOrder (Lex EmptyMonomial) = Nothing
    moTotalOrder (Lex (PlainMonomial xs)) = Just $ sum xs
    moAlign n (Lex EmptyMonomial) = Lex EmptyMonomial
    moAlign n (Lex (PlainMonomial xs)) = Lex $ PlainMonomial $ take n (xs ++ (repeat 0))
    moMake p = Lex p
    moGetPlain (Lex p) = p
    moShow l = show l

instance Eq GrLex where
    (GrLex x) == (GrLex y) = x == y

instance Ord GrLex where
    compare (GrLex (PlainMonomial [])) _ = error "Invalid PlainMonomial"
    compare _ (GrLex (PlainMonomial [])) = error "Invalid PlainMonomial"
    compare (GrLex EmptyMonomial) (GrLex EmptyMonomial) = EQ
    compare (GrLex EmptyMonomial) _ = LT
    compare _ (GrLex EmptyMonomial) = GT
    compare (GrLex xa@(PlainMonomial xs)) (GrLex ya@(PlainMonomial ys)) = if sum xs == sum ys
                                                                          then compare (Lex xa) (Lex ya)
                                                                          else compare (sum xs) (sum ys)

instance MultiDeg GrLex where
    moPlus _ (GrLex EmptyMonomial) = GrLex EmptyMonomial
    moPlus (GrLex EmptyMonomial) _ = GrLex EmptyMonomial
    -- moPlus (GrLex (PlainMonomial xs)) (GrLex (PlainMonomial ys)) = GrLex $ PlainMonomial $ (+) <$> xs <*> ys
    moPlus (GrLex (PlainMonomial xs)) (GrLex (PlainMonomial ys)) = GrLex $ PlainMonomial $ zipWith (+) (xs ++ (repeat 0)) (ys ++ (repeat 0))
    moNeg (GrLex EmptyMonomial) = error "No reciprocal of EmptyMonomial! It's just the zero division!!"
    moNeg (GrLex (PlainMonomial xs)) = GrLex $ PlainMonomial $ negate <$> xs
    moEmpty = GrLex EmptyMonomial
    moZero = GrLex $ PlainMonomial $ repeat 0
    moTotalOrder (GrLex EmptyMonomial) = Nothing
    moTotalOrder (GrLex (PlainMonomial xs)) = Just $ sum xs
    moAlign n (GrLex EmptyMonomial) = GrLex EmptyMonomial
    moAlign n (GrLex (PlainMonomial xs)) = GrLex $ PlainMonomial $ take n (xs ++ (repeat 0))
    moMake p = GrLex p
    moGetPlain (GrLex p) = p
    moShow l = show l


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

texRational :: Rational -> String
texRational r = if denominator r == 1
                then if numerator r == 1
                     then ""
                     else negativeParen r $ show $ numerator r
                else negativeParen r $ "\\frac{" ++ (show $ numerator r) ++ "}{" ++ (show $ denominator r) ++ "}"

texConstRational :: Rational -> String
texConstRational r = if denominator r == 1
                then negativeParen r $ show $ numerator r
                else negativeParen r $ "\\frac{" ++ (show $ numerator r) ++ "}{" ++ (show $ denominator r) ++ "}"

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
                               
          
data DivisionState coef multideg = DivisionState {dividend :: Poly coef multideg,
                                                  divisors :: [((Poly coef multideg),(Poly coef multideg))],
                                                  stock :: Poly coef multideg,
                                                  remainder :: Poly coef multideg,
                                                  divLog :: [DivisionLog coef multideg]}

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
                                                       
                                                                  
                                     
texIndeterminate :: String -> Int -> String
texIndeterminate s n = case n of
                         0 -> ""
                         1 -> s
                         otherwise -> s ++ "^{" ++ (show n)  ++ "}"

texMultideg :: (MultiDeg multideg) => Int -> multideg -> String
texMultideg n d = let p = moGetPlain $ d
                  in if p == EmptyMonomial
                     then "0"
                     else foldr (++) "" $ zipWith (texIndeterminate) (chooseChars n) (getMultidegList p)
                               
--(map (("^" ++).show) $ getMultidegList p)

texTerm :: (MultiDeg multideg) => Int -> Term Rational multideg -> String
texTerm n Term{coef=coef, multideg=multideg} = if moAlign n multideg == moAlign n moZero
                                               then texConstRational coef
                                               else (texRational coef) ++ (texMultideg n multideg)

texPoly :: (MultiDeg multideg) => Poly Rational multideg -> String
texPoly (Poly n terms) = let temp = intercalate "+" $ map (texTerm n) terms
                         in if temp == ""
                            then "0"
                            else temp

texAlignPoly :: (MultiDeg multideg) => Poly Rational multideg -> String
texAlignPoly p = execWriter $ do
                   tell "\\begin{align}\n"
                   tell $ texPoly p
                   tell "\n"
                   tell $ "\\end{align}\n"

texBraceDoller :: String -> String
texBraceDoller s = "$" ++ s ++ "$"

texDivisionLog :: (MultiDeg multideg) => (DivisionLog Rational multideg) -> String
texDivisionLog p = case p of DLogDivides divisor stock -> execWriter $ do
                                                            tell "Division: $"
                                                            tell $ texPoly divisor
                                                            tell "$ divides stock. stock is $"
                                                            tell $ texPoly stock
                                                            tell $ "$ .  "
                             DLogMove moved -> execWriter $ do
                                                      tell "Remainder: $"
                                                      tell $ texPoly moved
                                                      tell $ "$ moved to remainder.  "
                             DLogCompleted qs r -> execWriter $ do
                                                     tell "Completed: quotients are "
                                                     tell "\\begin{itemize}\n"
                                                     sequence $ map (tell.(\x -> "\\item " ++ x ++ ", \n").texBraceDoller.texPoly) qs
                                                     tell "\\end{itemize} .  \n"
                                                     tell "remainder is "
                                                     tell $ (texBraceDoller.texPoly) r
                                                     tell $ ".  $\\blacksquare$"
                             DLogStart f gs -> execWriter $ do
                                                 tell "Start: calculates "
                                                 tell $ texBraceDoller $ (texPoly f) ++ "\\div "
                                                 tell "\\begin{itemize}\n"
                                                 sequence $ map (tell.(\x -> "\\item " ++ x ++ ", \n").texBraceDoller.texPoly) gs
                                                 tell "\\end{itemize} .  \n"

                                                     
                                                            
                               

texDivisionLogs ::(MultiDeg multideg) => [DivisionLog Rational multideg] -> String
texDivisionLogs ps = execWriter $ do
                       tell "\\begin{enumerate}\n"
                       sequence $ map tell $ map (\x -> "\\item " ++ x ++ "\n") $ map texDivisionLog ps
                       tell "\\end{enumerate}\n"

texTellDocumentStart :: Writer String ()
texTellDocumentStart = do
  tell "\\documentclass{jsarticle}\n"
  tell "\\usepackage{amsmath,amssymb,amsfonts}\n"
  tell "\\begin{document}\n"
  return ()

texTellDocumentEnd :: Writer String ()
texTellDocumentEnd = do
  tell "\\end{document}\n"
  return ()

makeLexPoly' :: Int -> (Rational, [Int]) -> Term Rational Lex
makeLexPoly' n (r, xs) = Term r (Lex (PlainMonomial xs))

makeLexPoly :: Int -> [(Rational, [Int])] -> Poly Rational Lex
makeLexPoly n xs = Poly n $ map (makeLexPoly' n) xs

makePoly' :: (MultiDeg multideg) => Int -> (Rational, [Int]) -> Term Rational multideg
makePoly' n (r, xs) = Term r (moMake (PlainMonomial xs))

makePoly :: (MultiDeg multideg) => Int -> [(Rational, [Int])] -> Poly Rational multideg
makePoly n xs = normalize $ Poly n $ map (makePoly' n) xs

main' = do
  -- putStrLn $ show $ (Lex (PlainMonomial [1])) `moPlus` (Lex (PlainMonomial [2]))
  let p = Poly 2 [Term (3%1) (Lex (PlainMonomial [1,2])), Term (4%1) (Lex (PlainMonomial [1,2])), Term (1%1) (Lex (PlainMonomial [1,1]))]
  let q = Poly 2 [Term (3%1) (Lex (PlainMonomial [1,2])), Term (1%1) (Lex (PlainMonomial [1,1]))]
  let r = Poly 2 [Term (3%1) (Lex (PlainMonomial [1,2])), zeroTerm, constTerm (2%1), constTerm (3%2)]
  let z = Poly 2 [zeroTerm :: (Term Rational Lex)]
  let a = Poly 1 [Term (2%1) (Lex (PlainMonomial [1])), Term (2%1) (Lex $ PlainMonomial [0])]
  let b = Poly 1 [Term (1%1) (Lex (PlainMonomial [1])), Term (1%1) (Lex $ PlainMonomial [0])]
  let c = makeLexPoly 1 [(1%1, [2]), (2%1, [1]), (1%1, [0])]
  let d = makeLexPoly 1 [(1%1, [1]), (1%1, [0])]
  let t1 = [makeLexPoly 2 [(1%1, [1,2]), (1%1, [0,0])],
            makeLexPoly 2 [(1%1, [1,1]), (1%1, [0,0])],
            makeLexPoly 2 [(1%1, [0,1]), (1%1, [0,0])]]
  let t3 = [makeLexPoly 2 [(1%1, [2,1]), (1%1, [1,2]), (1%1, [0,2])],
            makeLexPoly 2 [(1%1, [1,1]), (-1%1, [0,0])],
            makeLexPoly 2 [(1%1, [0,2]), (-1%1, [0,0])]]
  let tt = [[(1%1, [7,2]), (1%1, [3,2]), (-1%1, [0,1]), (1%1, [0,0])],
            [(1%1, [1,2]), (-1%1, [1,0])],
            [(1%1, [1,0]), (-1%1, [0,3])]]
  let t4 = (map (makePoly 2) tt) :: [Poly Rational Lex]
  let t5 = (map (makePoly 2) tt) :: [Poly Rational GrLex]
  let t6 = [t4!!0, t4!!2, t4!!1]
  let t7 = [t5!!0, t5!!2, t5!!1]
  -- putStrLn $ show $ p
  -- --putStrLn $ show $ simplify p
  -- putStrLn $ show $ normalize p
  -- --putStrLn $ show $ simplify r
  -- putStrLn $ show $ normalize r
  -- putStrLn $ show $ normalize z
  -- putStrLn $ texPoly $ normalize p
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ texAlignPoly $ normalize p
         tell $ texAlignPoly $ normalize q
         tell $ texAlignPoly $ normalize r
         tell $ texAlignPoly $ normalize z
         tell $ texAlignPoly $ normalize $ p*r
         tell $ texAlignPoly $ normalize $ Poly 5 [Term (3%1) (Lex (PlainMonomial [1,2,3,4,5])), Term (4%1) (Lex (PlainMonomial [1,2,7,3,1])), Term (1%1) (Lex (PlainMonomial [1,1]))]
         tell $ show $ moAlign 3 $ (Lex (PlainMonomial [1,2])) `moPlus` (Lex (PlainMonomial [2]))
         tell $ texAlignPoly $ normalize $ a
         tell $ texAlignPoly $ normalize $ negate b
         -- tell $ show $  normalize $ a + negate b
         tell $ texAlignPoly $ normalize $ a + negate b
         tell $ texAlignPoly $ normalize $ remainder $ divide c [d]
         tell $ texDivisionLogs $ reverse.divLog $ divide c [d]
         tell $ texAlignPoly $ normalize $ head t1
         tell $ texAlignPoly $ normalize $ t1!!1
         tell $ texAlignPoly $ normalize $ t1!!2
         let t2 = makeLexPoly 2 [((-1%1), [0,1]), (1%1, [0,0])]
         tell $ show $ getMultideg $ t2
         tell $ show $ getMultideg $ (t1!!2)
         tell $ show $ (t1!!2) `polyDivides` t2
         tell $ texAlignPoly $ normalize $ remainder $ divide (head t1) (tail t1)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t1) (tail t1)
         tell $ "\\newpage{}"
         --tell $ texAlignPoly $ normalize $ remainder $ divide (head t3) (tail t3)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t3) (tail t3)
         --tell $ texAlignPoly $ normalize $ remainder $ divide (head t3) (reverse $ tail t3)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t3) (reverse $ tail t3)
         tell $ texDivisionLogs $ reverse.divLog $ divide c [d]
         tell $ "\\newpage{}"         
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t4) (tail t4)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t5) (tail t5)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t6) (tail t6)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t7) (tail t7)
         texTellDocumentEnd
    
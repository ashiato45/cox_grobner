module TeXPrinter where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Distribution.Simple.Utils
import MultiDeg
import MultiPoly
import Groebner


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
texDivisionLog p =
  case p of
    DLogDivides divisor stock ->
      execWriter $ do
        tell "Division: $"
        tell $ texPoly divisor
        tell "$ divides stock. stock is $"
        tell $ texPoly stock
        tell $ "$ .  "
    DLogMove moved ->
      execWriter $ do
        tell "Remainder: $"
        tell $ texPoly moved
        tell $ "$ moved to remainder.  "
    DLogCompleted qs r ->
      execWriter $ do
        tell "Completed: quotients are "
        tell "\\begin{itemize}\n"
        _ <- sequence $ map (tell.(\x -> "\\item " ++ x ++ ", \n").texBraceDoller.texPoly) qs
        tell "\\end{itemize} .  \n"
        tell "remainder is "
        tell $ (texBraceDoller.texPoly) r
        tell $ ".  $\\blacksquare$"
    DLogStart f gs ->
      execWriter $ do
        tell "Start: calculates "
        tell $ texBraceDoller $ (texPoly f) ++ "\\div "
        tell "\\begin{itemize}\n"
        _ <- sequence $ map (tell.(\x -> "\\item " ++ x ++ ", \n").texBraceDoller.texPoly) gs
        tell "\\end{itemize} .  \n"





texDivisionLogs ::(MultiDeg multideg) => [DivisionLog Rational multideg] -> String
texDivisionLogs ps = execWriter $ do
                       tell "\\begin{enumerate}\n"
                       _ <- sequence $ map tell $ map (\x -> "\\item " ++ x ++ "\n") $ map texDivisionLog ps
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


texSPairCheckLog' :: (MultiDeg multideg) =>
  SPairCheckLog Rational multideg -> String
texSPairCheckLog' (SPCLogDivision p1_ p2_ divisor_ spoly_ remainder_ log_) =
  execWriter $ do
    tell $ texBraceDoller $ execWriter $ do
      tell "S("
      tell $ texPoly p1_
      tell ", "
      tell $ texPoly p2_
      tell ") = "
      tell $ texPoly spoly_
      return ()
    tell ".  "
    tell "Calculation is "
    tell $ texDivisionLogs $ reverse $ log_
    tell ".  "
    tell $ texBraceDoller "\\blacksquare{}"
    return ()


texSPairCheckLog :: (MultiDeg multideg) =>
  [SPairCheckLog Rational multideg] -> String
texSPairCheckLog xs = execWriter $ do
  tell "Checking if following bases are Groebner basis.  \n"
  tell "Divisors and bases are \n"
  tell "\\begin{itemize}\n"
  sequence $ map tell $ map (\x -> "\\item " ++ (texBraceDoller.texPoly) x) $ (spcDivisor $ xs!!0)
  tell "\\end{itemize} .  \n"
  tell "\\begin{enumerate}\n"
  _ <- sequence $ map tell $ map (\x -> "\\item " ++ texSPairCheckLog' x) xs
  tell "\\end{enumerate}\n"
  return ()

main' :: IO ()
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

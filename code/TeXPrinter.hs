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

texItemize :: [String] -> String
texItemize xs = execWriter $ do
  tell "\\begin{itemize}\n"
  sequence $ map tell $ map (\x -> "\\item " ++ x ++ "\n") xs
  tell "\\end{itemize}  . \n"
  return ()

texCalcGroebnerLog' :: (MultiDeg multideg) =>
  CGroebnerLog Rational multideg -> String
texCalcGroebnerLog' (CGLogStart {cglInit = i_}) =
  execWriter $ do
    tell "Calculates groebner basis of \n"
    tell $ texItemize $ map (texBraceDoller.texPoly) i_
    tell "\n"
    return ()
texCalcGroebnerLog' (CGLogCalcSPoly {cglP1 = p1_, cglP2 = p2_, cglRemainder = r_}) =
  execWriter $ do
    tell "$\\overline{S("
    tell $ texPoly p1_
    tell ", "
    tell $ texPoly p2_
    tell $ ")} = "
    tell $ texPoly r_
    tell $ "$.  \n"
    return ()
texCalcGroebnerLog' (CGLogAppend {cglAppend = as_}) =
  execWriter $ do
    tell "Not enough.  Appends "
    tell $ texItemize $ map (texBraceDoller.texPoly) as_
    tell "\n"
texCalcGroebnerLog' (CGLogCompleted {cglCompleted = cs_}) =
  execWriter $ do
    tell "Enough for groebner basis.  Result is "
    tell $ texItemize $ map (texBraceDoller.texPoly) cs_
    tell $ texBraceDoller "\\blacksquare{}"

texCalcGroebnerLog :: (MultiDeg multideg) =>
  [CGroebnerLog Rational multideg] -> String
texCalcGroebnerLog = unlines.(map texCalcGroebnerLog')

texMinimalizeGroebnerLog' :: (MultiDeg multideg) =>
  MGroebnerLog Rational multideg -> String
texMinimalizeGroebnerLog' (MGLogStart is_) =
  execWriter $ do
    tell "Minimalizes groebner basis \n"
    tell $ texItemize $ map (texBraceDoller.texPoly) is_
    tell "\n"
    return ()
texMinimalizeGroebnerLog' (MGLogRemove removed_ removing_) =
  execWriter $ do
    tell $ texBraceDoller.texPoly $ removed_
    tell " is removed by "
    tell $ texBraceDoller.texPoly $ removing_
    tell ".  \n"
texMinimalizeGroebnerLog' (MGLogCompleted cs_) =
  execWriter $ do
    tell $ "Minimalized groebner basis is \n"
    tell $ texItemize $ map (texBraceDoller.texPoly) cs_
    tell "$\\blacksquare{}$"
    tell "\n"
    return ()

texMinimalizeGroebnerLog :: (MultiDeg multideg) =>
  [MGroebnerLog Rational multideg] -> String
texMinimalizeGroebnerLog = unlines.(map texMinimalizeGroebnerLog')

texReduceGroebnerLog' :: (MultiDeg multideg) =>
  RGroebnerLog Rational multideg -> String
texReduceGroebnerLog' (RGLogStart is_) =
  execWriter $ do
    tell "Reduce groebner basis \n"
    tell $ texItemize $ map (texBraceDoller.texPoly) is_
    tell "\n"
    return ()
texReduceGroebnerLog' (RGLogReduce before_ after_)=
  execWriter $ do
    tell $ "Reducing: $\\overline{"
    tell $ texPoly before_
    tell $ "} = "
    tell $ texPoly after_
    tell $ "$.  \n"
texReduceGroebnerLog' (RGLogCompleted cs_) =
  execWriter $ do
    tell "Reduced groebner basis is \n"
    tell $ texItemize $ map (texBraceDoller.texPoly) cs_
    tell "$\\blacksquare{}$"
    tell "\n"
    return ()

texReduceGroebnerLog :: (MultiDeg multideg) =>
  [RGroebnerLog Rational multideg] -> String
texReduceGroebnerLog = unlines.(map texReduceGroebnerLog')

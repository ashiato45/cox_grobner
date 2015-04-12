import           Control.Monad.Writer
import           Data.Ratio
import           MultiPoly
import MultiDeg
import Groebner
import TeXPrinter

calcPrintGroebnerAll :: (MultiDeg multideg) =>
  [Poly Rational multideg] -> String
calcPrintGroebnerAll xs = execWriter $ do
  let a = runWriter $ calcGroebner xs
  let b = runWriter $ minimalizeGroebner $ fst a
  let c = runWriter $ reduceGroebner $ fst b
  tell $ texCalcGroebnerLog $ snd a
  tell $ texMinimalizeGroebnerLog $ snd b
  tell $ texReduceGroebnerLog $ snd c

main :: IO ()
main = do
  let t1 = makePoly 2 [(1%1, [3,0]), (-2%1, [1,1])] :: Poly Rational GrLex
  let t2 = makePoly 2 [(1%1, [2,1]), (-2%1, [0,2,0]), (1%1, [1,0])] :: Poly Rational GrLex
  let tt = [[(1%1, [2,0]), (1%1, [1,1])],
            [(1%1, [1,1])],
            [(1%1, [0,2]), (-1%2, [1,0])]]
  let tt3 = map (makePoly 2) tt :: [Poly Rational GrLex]
  let a1 = [[(1%1, [2, 1]), (-1%1, [0,0])],
            [(1%1, [1, 2]), (-1%1, [1,0])]]
  let b1 = map (makePoly 2) a1 :: [Poly Rational Lex]
  let b1' = map (makePoly 2) a1 :: [Poly Rational GrLex]
  let a2 = [[(1%1, [2,0]), (1%1, [0,1])],
            [(1%1, [4,0]), (2%1, [2,1]), (1%1, [0,2]), (3%1, [0,1])]]
  let b2 = map (makePoly 2) a2 :: [Poly Rational Lex]
  let b2' =  map (makePoly 2) a2 :: [Poly Rational GrLex]
  let a3 = [[(1%1, [1,0,0]), (-1%1, [0,0,4])],
            [(1%1, [0,1,0]), (-1%1, [0,0,5])]]
  let b3 = map (makePoly 3) a3 :: [Poly Rational Lex]
  let b3' = map (makePoly 3) a3 :: [Poly Rational GrLex]
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ "\\section{}"
         let c = runWriter $ calcGroebner [t1, t2]
         tell $ texCalcGroebnerLog $ snd $ c
         let m = runWriter $ minimalizeGroebner $ fst c
         tell $ texMinimalizeGroebnerLog $ snd m
         tell $ texReduceGroebnerLog $ execWriter $ reduceGroebner $ fst m
         tell $ texReduceGroebnerLog $ execWriter $ reduceGroebner tt3
         tell $ "\\section{}"
         let c1 = runWriter $ calcGroebner b1
         tell $ texCalcGroebnerLog $ snd $ c1
         let d1 = runWriter $ minimalizeGroebner $ fst c1
         tell $ texMinimalizeGroebnerLog $ snd d1
         let e1 = runWriter $ reduceGroebner $ fst d1
         tell $ texReduceGroebnerLog $ snd e1
         tell $ "\\section{}"
         let c1' = runWriter $ calcGroebner b1'
         tell $ texCalcGroebnerLog $ snd $ c1'
         let d1' = runWriter $ minimalizeGroebner $ fst c1'
         tell $ texMinimalizeGroebnerLog $ snd d1'
         let e1' = runWriter $ reduceGroebner $ fst d1'
         tell $ texReduceGroebnerLog $ snd e1'
         tell $ "\\section{b-lex}"
         tell $ calcPrintGroebnerAll b2
         tell $ "\\section{b-grlex}"
         tell $ calcPrintGroebnerAll b2'
         tell $ "\\section{c-lex}"
         tell $ calcPrintGroebnerAll b3
         tell $ "\\section{c-grlex}"
         tell $ calcPrintGroebnerAll b3'

         texTellDocumentEnd

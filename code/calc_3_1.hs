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
  let a1 = [[(1%1, [1, 1, 0]), (-1%1, [0,0])],
            [(1%1, [1, 0, 1]), (-1%1, [0,0])]]
  let b1 = map (makePoly 3) a1 :: [Poly Rational Lex]
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ "\\section{are}"
         tell $ calcPrintGroebnerAll b1

         texTellDocumentEnd

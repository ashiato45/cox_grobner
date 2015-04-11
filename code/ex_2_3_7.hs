import           Control.Monad.Writer
import           Data.Ratio
import           MultiPoly
import MultiDeg
import Groebner
import TeXPrinter

main :: IO ()
main = do
  let t1 = makePoly 2 [(1%1, [3,0]), (-2%1, [1,1])] :: Poly Rational GrLex
  let t2 = makePoly 2 [(1%1, [2,1]), (-2%1, [0,2,0]), (1%1, [1,0])] :: Poly Rational GrLex
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ texCalcGroebnerLog $ execWriter $ calcGroebner [t1, t2]
         texTellDocumentEnd

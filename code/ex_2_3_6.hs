import           Control.Monad.Writer
import           Data.Ratio
import           MultiPoly
import MultiDeg
import Groebner
import TeXPrinter

main :: IO ()
main = do
  let t1 = makePoly 3 [(1%1, [1,0,0]), (-1%1, [0,0,2])] :: Poly Rational Lex
  let t2 = makePoly 3 [(1%1, [0,1,0]), (-1%1, [0,0,3])] :: Poly Rational Lex
  let t3 = makePoly 3 [(1%1, [2,0,0]), (-1%1, [0,1,0])] :: Poly Rational GrLex
  let t4 = makePoly 3 [(1%1, [3,0,0]), (-1%1, [0,0,1])] :: Poly Rational GrLex
  let t5 = makePoly 3 [(1%1, [2,0,0]), (-1%1, [0,1,0])] :: Poly Rational InvLex
  let t6 = makePoly 3 [(1%1, [3,0,0]), (-1%1, [0,0,1])] :: Poly Rational InvLex
  let ts = [
            [(1%1, [1,2,0]), (-1%1, [1,0,1]), (1%1, [0,1,0])],
            [(1%1, [1,1,0]), (-1%1, [0,0,2])],
            [(1%1, [1,0,0]), (-1%1, [0,1,4])]
           ]
  let tss = map (makePoly 3) ts :: [Poly Rational Lex]
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ texAlignPoly $ lcmPoly t1 t2
         tell $ texAlignPoly $ calcSPoly t1 t2
         tell $ texSPairCheckLog $ reverse.execWriter $ sPairCheck [t1, t2]
         tell $ texSPairCheckLog $ reverse.execWriter $ sPairCheck [t3, t4]
         tell $ texSPairCheckLog $ reverse.execWriter $ sPairCheck [t5, t6]
         tell $ texSPairCheckLog $ execWriter $ sPairCheck tss
         texTellDocumentEnd

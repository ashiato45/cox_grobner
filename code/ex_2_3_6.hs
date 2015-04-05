import           Control.Monad.Writer
import           Data.Ratio
import           MultiPoly

main :: IO ()
main = do
  let t1 = makePoly 3 [(1%1, [1,0,0]), (-1%1, [0,0,2])] :: Poly Rational Lex
  let t2 = makePoly 3 [(1%1, [0,1,0]), (-1%1, [0,0,3])] :: Poly Rational Lex
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ texAlignPoly $ lcmPoly t1 t2
         tell $ texAlignPoly $ calcSPoly t1 t2
         texTellDocumentEnd

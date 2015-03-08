import MultiPoly
import Data.Ratio
import Control.Monad.Writer    

main = do
  let tt = [[(1%1, [3,0,0]), (-1%1, [2,1,0]), (-1%1, [2,0,1]), (1%1, [1,0,0])],
            [(1%1, [2,1,0]), (-1%1, [0,0,1])],
            [(1%1, [1,1,0]), (-1%1, [0,0,0])]]
  let t1 = (map (makePoly 3) tt) :: [Poly Rational GrLex]
  let t2 = [t1!!0, t1!!2, t1!!1]
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t1) (tail t1)
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t2) (tail t2)
         texTellDocumentEnd

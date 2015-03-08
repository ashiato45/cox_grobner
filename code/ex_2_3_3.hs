import MultiPoly
import Data.Ratio
import Control.Monad.Writer    

main = do
  let tt = [[(1%1, [7,2]), (1%1, [3,2]), (-1%1, [0,1]), (1%1, [0,0])],
            [(1%1, [1,2]), (-1%1, [1,0])],
            [(1%1, [1,0]), (-1%1, [0,3])]]
  let t4 = (map (makePoly 2) tt) :: [Poly Rational Lex]
  let t5 = (map (makePoly 2) tt) :: [Poly Rational GrLex]
  let t6 = [t4!!0, t4!!2, t4!!1]
  let t7 = [t5!!0, t5!!2, t5!!1]
  putStrLn $ execWriter $ do
         texTellDocumentStart
         tell $ "\\begin{description}"
         tell $ "\\item[1a,grlex]"
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t5) (tail t5)
         tell $ "\\item[1a,lex]"
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t4) (tail t4)
         tell $ "\\item[1b,grlex]"
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t7) (tail t7)
         tell $ "\\item[1b,lex]"
         tell $ texDivisionLogs $ reverse.divLog $ divide (head t6) (tail t6)              
         tell $ "\\end{description}"
         texTellDocumentEnd

import Math.Polynomial 
import Data.Ratio

embed :: Integer -> Rational
embed n = n % 1

combineTuple :: (Show a) => (a, String) -> String
combineTuple (x, y) = "(" ++ show x ++ ")" ++ y

intervene :: a -> [a] -> [a]
intervene x [] = []
intervene x [y] = [y]
intervene x (y:ys) = y:x:(intervene x ys)

showPoly :: (Eq a,Num a,Show a) => Poly a -> String
showPoly p = let c = polyCoeffs LE p
                 sym = zipWith (++) (repeat "x^") $ map show [0..]
             in foldl1 (++) $ intervene "+" $ reverse $ map combineTuple $ zip c sym

main = do
  line <- getLine
  let p = poly BE $ (map (embed.read)) $ words line
  let d = polyDeriv p
  let g = gcdPoly p d
  putStrLn $ showPoly $ quotPoly p g
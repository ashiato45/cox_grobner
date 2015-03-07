import Math.Polynomial
import Data.Ratio

embed :: Integer -> Rational
embed n = n % 1

main = do
  let p = map ((poly BE).(map embed)) [[1,0,1,0,1], [1,0,-1,-2,-1], [1,0,0,-1]]
  let q = map ((poly BE).(map embed)) [[1,2,1],[1,1]]
  putStrLn $ show $ foldl1 gcdPoly p
  putStrLn $ show $ foldl1 gcdPoly q

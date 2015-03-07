import Math.Polynomial
import Data.Ratio

embed :: Integer -> Rational
embed n = n % 1

main = do
  n <- getLine
  line <- sequence $ replicate (read n) getLine
  let p = map ((poly BE).(map (embed.(read :: String -> Integer)).words)) line
  putStrLn $ show $ foldl1 gcdPoly p

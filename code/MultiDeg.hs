module MultiDeg where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Distribution.Simple.Utils

data PlainMonomial = PlainMonomial [Int]|EmptyMonomial deriving (Show, Eq)
newtype Lex = Lex PlainMonomial deriving (Show, Eq)
newtype GrLex = GrLex PlainMonomial deriving (Show, Eq)
newtype InvLex = InvLex PlainMonomial deriving (Show, Eq)


class (Ord a, Show a) => MultiDeg a where
  moMake :: PlainMonomial -> a
  moGetPlain :: a -> PlainMonomial
  moPlus :: a -> a -> a
  moPlus a1 a2 = moMake $
    let
      pm1 = moGetPlain a1
      pm2 = moGetPlain a2
    in
      case (pm1, pm2) of
        (EmptyMonomial, EmptyMonomial) -> EmptyMonomial
        (PlainMonomial xs1, PlainMonomial xs2) ->
          PlainMonomial $ zipWith (+) xs1 xs2
        (_ , EmptyMonomial) -> pm1
        (EmptyMonomial, _) -> pm2
  moNeg :: a -> a
  moNeg a = moMake $
    let
      pm = moGetPlain a
    in
      case pm of
        EmptyMonomial -> error "No reciprocal of EmptyMonomial! It's just the zero division!!"
        PlainMonomial xs -> PlainMonomial $ negate <$> xs
  moMinus :: a -> a -> a
  moMinus x y = moPlus x $ moNeg y
  moEmpty :: a
  moEmpty = moMake EmptyMonomial
  moZero :: a
  moZero = moMake $ PlainMonomial $ repeat 0
  moTotalOrder :: a -> Maybe Int
  moTotalOrder a =
    case moGetPlain a of
      EmptyMonomial -> Nothing
      PlainMonomial xs -> Just $ sum xs
  moAlign :: Int -> a -> a
  moAlign n a = moMake $
    let
      pm = moGetPlain a
    in
      case pm of
        EmptyMonomial -> EmptyMonomial
        PlainMonomial xs -> PlainMonomial $ take n (xs ++ (repeat 0))
  moShow :: a -> String
  moShow = show
  moMax :: a -> a-> a
  moMax a1 a2 = moMake $
    let
      pm1 = moGetPlain a1
      pm2 = moGetPlain a2
    in
      case (pm1, pm2) of
        (PlainMonomial xs1, PlainMonomial xs2) -> PlainMonomial $ zipWith max xs1 xs2
        (PlainMonomial _, EmptyMonomial) -> pm1
        (EmptyMonomial, PlainMonomial _) -> pm2
        _ -> EmptyMonomial

trivialComparator :: (MultiDeg a) =>
  ([Int] -> [Int] -> Ordering) -> a -> a -> Ordering
trivialComparator f md1 md2 =
  let
    pm1 = moGetPlain md1
    pm2 = moGetPlain md2
  in
    case (pm1, pm2) of
      (PlainMonomial [], _) -> error "Invalid PlainMonomial"
      (_, PlainMonomial []) -> error "Invalid PlainMonomial"
      (EmptyMonomial, EmptyMonomial) -> EQ
      (EmptyMonomial, _) -> LT
      (_, EmptyMonomial) -> GT
      (PlainMonomial xs1, PlainMonomial xs2) -> f xs1 xs2

lexComparator :: [Int] -> [Int] -> Ordering
lexComparator [x] [y] = compare x y
lexComparator (x:xs) (y:ys) =
  if x == y
  then lexComparator xs ys
  else compare x y
lexComparator _ _ = error "lexComparator error!"

instance Ord Lex where
  compare = trivialComparator lexComparator

instance MultiDeg Lex where
  moMake p = Lex p
  moGetPlain (Lex p) = p

gradedComparator :: ([Int] -> [Int] -> Ordering) -> [Int] -> [Int] -> Ordering
gradedComparator f xs ys =
  if sum xs == sum ys
  then f xs ys
  else compare (sum xs) (sum ys)

instance Ord GrLex where
  compare = trivialComparator (gradedComparator lexComparator)

instance MultiDeg GrLex where
    moMake p = GrLex p
    moGetPlain (GrLex p) = p

invComparator :: ([Int] -> [Int] -> Ordering) -> [Int] -> [Int] -> Ordering
invComparator f xs ys = f (reverse xs) (reverse ys)

instance Ord InvLex where
  compare = trivialComparator (invComparator lexComparator)

instance MultiDeg InvLex where
  moMake p = InvLex p
  moGetPlain (InvLex p) = p

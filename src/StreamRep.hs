{-#LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable#-}
module StreamRep where

import Data.Functor.Rep
import Data.Distributive
-- | Solution to CTFP Chapter 14 Challenge 4
data Stream a = Cons a (Stream a) deriving (Show,Read,Functor,Eq)

instance Distributive Stream where
    distribute = distributeRep

instance Representable Stream where
    type Rep Stream = Int
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

square :: Int -> Int
square x = x * x

squares :: Stream Int
squares = tabulate square

calculateSquare  = index squares 

-- | Solution to CTFP Chapter 14 Challenge 6

data Pair a = Pair a a deriving (Show, Read, Functor, Eq)

instance Distributive Pair where
    distribute = distributeRep

instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) True = x
    index (Pair x y) False = y

toNum :: Bool -> Int
toNum True = 1
toNum False = 0

numPair :: Pair Int
numPair = tabulate toNum
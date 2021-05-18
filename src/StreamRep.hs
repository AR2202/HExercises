{-#LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable#-}
module StreamRep where

import Data.Functor.Rep
import Data.Distributive

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
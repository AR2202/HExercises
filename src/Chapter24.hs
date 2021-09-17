{-# LANGUAGE DeriveFunctor #-}

module Chapter24
( nPrimes
, nextSquare
, squares'
, Polynomial(..)
, evalPolynomial
  )
  where
--import Data.Functor.Foldable
import Data.List
--import Control.Monad.Fix





--Excersises Chapter 24
--Category Theory for programmers (Bartosz Milewski)

--------Types--------
newtype Fix f = Fix (f(Fix f))

data StreamF e a = StreamF e a
    deriving (Functor, Show)

data Ring =  RZero
           | ROne
           | RAdd Ring Ring
           | RMul Ring Ring
           | RNeg Ring
    deriving Show

data Polynomial =  Poly [Int] -- Polynomial literals represented as List of coefficients - empty list is Zero and [1] is One of the Ring
                 | PAdd Polynomial Polynomial
                 | PMult Polynomial Polynomial
                 | PNeg Polynomial
-- 1.------------------------------------------
evalPolynomial :: Polynomial -> [Int]
evalPolynomial (Poly xs)                = xs
evalPolynomial (PAdd p1 p2)             = sumPolynomials (evalPolynomial p1) (evalPolynomial p2)
evalPolynomial (PMult p1 p2)            = multiplyPolynomials (evalPolynomial p1) (evalPolynomial p2)
    where multiplyPolynomials [] _      = []
          multiplyPolynomials (x:xs) ys = sumPolynomials (fmap (x *) ys) (multiplyPolynomials xs (0:ys))
evalPolynomial (PNeg p)                 = fmap negate $ evalPolynomial p

sumPolynomials xs []          = xs
sumPolynomials [] xs          = xs
sumPolynomials (x:xs) (y:ys)  = (x+y): sumPolynomials xs ys
-- 4.------------------------------------------
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

nextSquare :: [Int] -> StreamF Int [Int]
nextSquare (x:xs) =  StreamF (x*x) xs

squares' :: [Int] -> Fix (StreamF Int)
squares' = ana nextSquare
--5.------------------------------------------
era' :: (Int,[Int]) -> Maybe (Int, (Int,[Int]))
era' (numprimes, p:ns) = if numprimes == 0  then Nothing else Just (p, (numprimes - 1, filter (notdiv p) ns))
    where notdiv p n = n `mod` p /= 0

nPrimes :: Int -> [Int]
nPrimes n = unfoldr era' (n, [2..])
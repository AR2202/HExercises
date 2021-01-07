{-#LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs, UndecidableInstances#-}

module TypeLevelFib (
    fib5,
    fib2,
    fib10,
    fib21

)
where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy

fib21 = print (natVal (Proxy :: Proxy (Fib 21)))

type family Fib n where
  Fib 0 = 0
  Fib 1 = 1
  Fib k = Fib (k - 2) + Fib (k - 1)

fib5 = Proxy :: Proxy (Fib 5)
fib2 = Proxy :: Proxy (Fib 2)
fib10 = Proxy :: Proxy (Fib 10)

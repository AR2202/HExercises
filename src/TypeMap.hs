{-#LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs, UndecidableInstances,PolyKinds#-}

module TypeMap(
 negate'
)
where
import GHC.Generics
import GHC.TypeLits as TL
import Data.Proxy


type family MapNegate  (xs :: [Bool]) :: [Bool] where
  MapNegate  '[] = '[]
  MapNegate  (True ': xs) = False ': MapNegate xs
  MapNegate (False ': xs) = True ': MapNegate xs

negate' = Proxy :: Proxy (MapNegate '[True,False])
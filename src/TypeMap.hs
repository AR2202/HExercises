{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeMap
  ( negate'
  ) where

import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits as TL

type family MapNegate (xs :: [Bool]) :: [Bool] where
  MapNegate '[] = '[]
  MapNegate (True ': xs) = False ': MapNegate xs
  MapNegate (False ': xs) = True ': MapNegate xs

negate' = Proxy :: Proxy (MapNegate '[ True, False])

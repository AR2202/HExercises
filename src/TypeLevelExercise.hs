{-#LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs, UndecidableInstances,PolyKinds#-}

module TypeLevelExercise(
 test1,
 test2,
test5,
test6,
test7,
test8,
test9,
test10,
Prod
)
where
import GHC.Generics
import GHC.TypeLits as TL
import Data.Proxy

type family Negate (x :: Bool) :: Bool where
  Negate True = False
  Negate False = True

type family Equals (a :: k) (b :: k) :: Bool where
  Equals a a = True
  Equals _ _ = False

type family And (a :: Bool) (b :: Bool) :: Bool where
  And True True = True
  And _    _    = False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or False False = False
  Or _     _ = True

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse True a _ = a
  IfThenElse False _ b = b

type family Catenate (x :: [k]) (y :: [k]) :: [k] where
  Catenate '[] ys = ys
  Catenate (x ': xs) ys = x ': (Catenate xs ys)

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = (f x) ': (Map f xs)

type family Filter (predicate :: k -> Bool) (list :: [k]) :: [k] where
  Filter predicate '[] = '[]
  Filter predicate (x ': xs) =
    Catenate (IfThenElse (predicate x) '[x] '[]) (Filter predicate xs)

type family Add1 x where
  Add1 x = x + 1

type family Elem (x::a) (ys::[a]) :: Bool where
  Elem _ '[] = False
  Elem x (y:ys) = Or (Equals x y) (Elem x ys)

type family MapNegate  (xs :: [Bool]) :: [Bool] where
  MapNegate  '[] = '[]
  MapNegate  (True ': xs) = False ': MapNegate xs
  MapNegate (False ': xs) = True ': MapNegate xs

type family MapDiffTo2020  (xs :: [Nat]) :: [Nat] where
  MapDiffTo2020  '[] = '[]
  MapDiffTo2020  (x ': xs) = (2020-x) ': (MapDiffTo2020 xs)
  
type family FilterElem (xs :: [Nat]) (ys :: [Nat]):: [Nat] where
  FilterElem '[] _ = '[]
  FilterElem _ '[] = '[]
  FilterElem  xs (y ': ys) =
    Catenate (IfThenElse (Elem y xs) '[y] '[]) (FilterElem xs ys)

type family Head (xs ::[a]) :: a where
  Head '[] = TypeError (Text "Head of empty (type level) list!")
  Head (x ': _) = x

type family Tail (xs :: [a]) :: [a] where
  Tail '[] = '[]
  Tail (_ ': xs) = xs

type family Product x y where
  Product x 1 = x
  Product _ 0 = 0
  Product 0 _ = 0
  Product 1 y = y
  Product x y = x + Product x (y -1)


type Inputlist = '[1721,979,366,299,675,1456]
type Difflist = FilterElem Inputlist (MapDiffTo2020 Inputlist)
type NumOne = Head Difflist
type NumTwo = Head (Tail Difflist)
type Prod = Product NumOne NumTwo


test1 = Proxy :: Proxy (IfThenElse False 1 0)
test2 = Proxy :: Proxy (Catenate '[True] '[False])
test3 = Proxy :: Proxy ( Negate False)
test4 = Proxy :: Proxy  (Add1 1)
test5 = Proxy :: Proxy (MapNegate '[True,False])
test6 = Proxy :: Proxy (MapDiffTo2020 '[20,2000])
test7 = Proxy :: Proxy (MapDiffTo2020 Inputlist)
test8 = Proxy :: Proxy (Elem 1 Inputlist)
test9 = Proxy :: Proxy (FilterElem Inputlist (MapDiffTo2020 Inputlist))
test10 = print(natVal(Proxy :: Proxy NumOne ) * natVal(Proxy :: Proxy NumTwo))
test11 = Proxy :: Proxy NumTwo

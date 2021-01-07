module Change
(giveChange,
giveChange'
)
where
import Data.List(maximumBy, unfoldr)
----------------------------------------------------
--An excercise on corecursion
----------------------------------------------------

------------------------------------------------------
--Coin datatype for standard UK coins
----------------------------------------------------
data Coin = OneP | TwoP | FiveP | TenP | TwentyP | FiftyP | Pound |Pound2 deriving (Show,Read,Eq)

-- | takes an amount of money (in Penny ) as its input and returns a list of coins to give as change
giveChange :: Int -> [Coin]
giveChange rest 
    |rest==0 = []
    |otherwise = coin : giveChange (rest-coinvalue)
        where coin = largestCoin rest
              coinvalue = coinValue coin

-- | returns the value of each coin (in Penny)
coinValue :: Coin -> Int
coinValue OneP = 1
coinValue TwoP = 2
coinValue FiveP = 5
coinValue TenP = 10
coinValue TwentyP = 20
coinValue FiftyP = 50
coinValue Pound = 100
coinValue Pound2 = 200

-- | returns the largest Coin that is less or equal to the amount of money given as input
largestCoin :: Int -> Coin
largestCoin money = maximumBy (\x y ->compare (coinValue x) (coinValue y))[c| c <- [OneP, TwoP, FiveP, TenP, TwentyP, FiftyP, Pound, Pound2], coinValue c <= money]

-- | returns nochtin if the change to be given is 0, Just (largetst coin, rest of change) otherwise
largestCoin' :: Int -> Maybe (Coin,Int)
largestCoin' 0 = Nothing
largestCoin' money = Just (coin, money - coinval)
    where coin = largestCoin money
          coinval = coinValue coin

-- | an implementation of giveChange using unfoldr
giveChange' :: Int ->[Coin]
giveChange' = unfoldr largestCoin' 

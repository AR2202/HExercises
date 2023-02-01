{-# LANGUAGE FlexibleContexts #-}

module Tutorial
  ( Code
  , Statement(..)
  , Var
  , printParser
  , assignParser
  , addParser
  , statementParser
  , whileParser
  , twentyfour
  , allSubstrings
  , powerset
  , selectionSort
  , mergeSort
  , quickSort
  , evalExpression
  , convertString
  , Tree(..)
  , bfprint
  , printTreeBF
  , fib
  , NestedList(..)
  , flattenNested
  , inOrderTraversalPrint
  , leftViewPrint
  , minimumSum
  , minimumSumFaster
  , autori
  , addLists
  , addLists'
  , examplegraph
  , Graph(..)
  , depthFirstWithStack
  , depthFirstRec
  , countConnectedComponents
  , breadthFirstWithQueue
  , topSort
  ) where

import           Data.Char                            (isAlpha, isNumber)
import           Data.List                            (permutations, (\\))
import           Data.List.Split                      (splitOn)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromJust, isJust)
import           Data.Sequence                        (Seq (..), (<|), (><),
                                                       (|>))
import qualified Data.Sequence                        as Seq
import qualified Data.Set                             as S
import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Number

-- part1 parser
type Code = [Statement]

data Statement
  = Print Var
  | Assign Var Int
  | Add Var Var
  | While Var Code
  deriving (Eq, Show)

type Var = String

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

assignParser :: Parser Statement
assignParser =
  Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> int)

addParser :: Parser Statement
addParser =
  Add <$> (many1 letter) <*>
  (many1 space >> char '+' >> many1 space >> many1 letter)

statementParser :: Parser Statement
statementParser =
  try printParser <|> try assignParser <|> try addParser <|> whileParser

whileParser :: Parser Statement
whileParser =
  While <$> (string "while" >> many1 space >> many1 letter) <*>
  (many1 space >> string "positive" >> many1 space >> char '(' >> many1 space >>
   many (statementParser <* many1 space) <* char ')')

codeParser :: Parser Code
codeParser = spaces >> many (statementParser <* many1 space) <* eof

-- part 2 code generator
main = do
  fileText <- readFile "input.txt"
  let parsed = parse codeParser "input.txt" fileText
  print parsed

twentyfour :: [Int] -> Bool
twentyfour [] = False
twentyfour xs = any (`go` []) (permutations xs)
  where
    go [] intermediates = 24 `elem` intermediates
    go (x:xs) [] = go xs [x]
    go (x:xs) intermediates =
      go xs $
      ($ x) <$> [(*), (+), (-), safediv, flip div, flip (-)] <*> intermediates

safediv a b =
  if b == 0
    then b `div` a
    else a `div` b

allSubstrings [] = [""]
allSubstrings (x:xs) = containingX (reverse (x : xs)) ++ allSubstrings xs
  where
    containingX []     = []
    containingX (x:xs) = reverse (x : xs) : containingX xs

powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map ((:) x) (powerset xs)

selectionSort :: [Int] -> [Int]
selectionSort []   = []
selectionSort list = minimum list : selectionSort (list \\ [minimum list])

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list =
  merge
    (mergeSort (take (halflist list) list))
    (mergeSort (drop (halflist list) list))
  where
    halflist l = length l `div` 2
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y = x : merge (xs) (y : ys)
      | x > y = y : merge (x : xs) ys

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) =
  quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

data Expression
  = EmptyExp
  | Num Int Expression Expression
  | Letter Char Expression
  | ParseError

parseExpression "" = EmptyExp
parseExpression (x:xs)
  | isNumber x =
    Num
      (read [x])
      (parseExpression (takeWhile (/= ']') xs))
      (parseExpression (dropWhile (/= ']') xs))
  | x == ']' = parseExpression xs
  | x == '[' = parseExpression xs
  | isAlpha x = Letter x (parseExpression xs)
  | otherwise = ParseError

evalExpression EmptyExp = ""
evalExpression (Letter c e) = c : (evalExpression e)
evalExpression ParseError = "ParseError"
evalExpression (Num i e1 e2) =
  concat (replicate i (evalExpression e1)) ++ evalExpression e2

convertString = evalExpression . parseExpression

data Tree a
  = Nil
  | Node a (Tree a) (Tree a)

printTreeBF tree = bfprint [tree]

bfprint [] = return ()
bfprint trees = do
  let currentnodes = getCurrentnodes trees
  let nexttrees = getNextTrees trees
  mapM_ putStrLn currentnodes
  putStrLn ""
  bfprint nexttrees
  where
    getCurrentnode (Node x l r) = show x
    getCurrentnode Nil          = ""
    getCurrentnodes = map getCurrentnode
    getSubtrees Nil          = []
    getSubtrees (Node x l r) = [l, r]
    getNextTrees = concatMap getSubtrees

fib = (map fib' [0 ..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)

data NestedList a
  = List [a]
  | Nested [NestedList a]

flattenNested (List [])       = []
flattenNested (List list)     = list
flattenNested (Nested [])     = []
flattenNested (Nested (x:xs)) = flattenNested x ++ flattenNested (Nested xs)

inOrderTraversalPrint :: Show a => Tree a -> String
inOrderTraversalPrint Nil = ""
inOrderTraversalPrint (Node n l r) =
  inOrderTraversalPrint l ++ show n ++ inOrderTraversalPrint r

leftViewPrint :: Show a => Tree a -> String
leftViewPrint Nil  = ""
leftViewPrint tree = leftViewPrint' [tree]

leftViewPrint' :: Show a => [Tree a] -> String
leftViewPrint' [] = ""
leftViewPrint' trees = leftmostNode trees ++ leftViewPrint' (subtrees trees)
  where
    leftmostNode []              = ""
    leftmostNode (Nil:xs)        = leftmostNode xs
    leftmostNode (Node n l r:xs) = show n
    subtrees = concatMap sub
    sub Nil          = []
    sub (Node n l r) = [l, r]

minimumSum :: [Int] -> Int
minimumSum [] = 0
minimumSum (x:xs) = minimum [containingX (reverse (x : xs)), minimumSum xs]
  where
    containingX []     = 0
    containingX (x:xs) = minimum [sum (x : xs), containingX xs]

minimumSumFaster :: [Int] -> Int
minimumSumFaster [] = 0
minimumSumFaster (x:xs) =
  minimum $
  scanl1
    (\acc y ->
       if acc < 0
         then acc + y
         else y)
    (x : xs)

autori :: String -> String
autori "" = ""
autori s  = map head $ splitOn "-" s

data ListNode a
  = NilNode
  | Val a (ListNode a)

addLists :: ListNode Int -> ListNode Int -> ListNode Int
addLists = addLists' 0

addLists' :: Int -> ListNode Int -> ListNode Int -> ListNode Int
addLists' 0 NilNode x = x
addLists' 0 x NilNode = x
addLists' carried NilNode x = addLists' 0 (Val carried NilNode) x
addLists' carried x NilNode = addLists' 0 x (Val carried NilNode)
addLists' carried (Val x nextx) (Val y nexty) =
  Val added (addLists' newcarried nextx nexty)
  where
    added = (x + y + carried) `mod` 10
    newcarried = (x + y + carried) `div` 10

type Graph = M.Map Char [Char]

examplegraph =
  M.fromList
    [ ('a', ['b', 'c'])
    , ('b', ['d'])
    , ('c', ['e'])
    , ('d', ['f'])
    , ('e', [])
    , ('f', [])
    ]

depthFirstWithStack graph source = go graph [source]
  where
    go graph []     = ""
    go graph (x:xs) = x : go graph (M.findWithDefault "" x graph ++ xs)

depthFirstRec graph [] = ""
depthFirstRec graph nodes =
  concatMap (\y -> y : depthFirstRec graph (M.findWithDefault "" y graph)) nodes

countConnectedComponents graph = go graph (M.keys graph) S.empty 0
  where
    go graph [] visited count = count
    go graph (x:xs) visited count
      | x `S.member` visited = go graph xs visited count
      | otherwise =
        go
          graph
          xs
          (foldr S.insert visited (depthFirstRec graph [x]))
          (count + 1)

breadthFirstWithQueue graph source = go graph (Seq.singleton source)
  where
    go graph Seq.Empty = ""
    go graph (x :<| xs) =
      x : go graph (xs >< (Seq.fromList $ M.findWithDefault "" x graph))

topSort graph = go graph (M.keys graph) S.empty
  where
    go graph [] visited = []
    go graph (x:xs) visited
      | x `S.member` visited = go graph xs visited
      | otherwise =
        x : go graph (M.findWithDefault [] x graph ++ xs) (S.insert x visited)

data LRUcache =
  LRUcache
    { cached    :: M.Map Int Int
    , cachorder :: Seq.Seq Int
    , capacity  :: Int
    }

getLRU :: Int -> LRUcache -> (Int, LRUcache)
getLRU key cache =
  case M.lookup key (cached cache) of
    Nothing -> (-1, cache)
    Just i -> (i, updatedCache)
      where updatedCache =
              cache {cachorder = key :<| Seq.filter (/= key) (cachorder cache)}

putLRU :: Int -> Int -> LRUcache -> LRUcache
putLRU key val cache
  | M.member key (cached cache) =
    cache {cachorder = filteredOrder, cached = newcached}
  | capacity cache > 0 =
    cache
      { cachorder = filteredOrder
      , cached = newcached
      , capacity = capacity cache - 1
      }
  | Seq.null (cachorder cache) = cache
  | otherwise =
    cache
      { cachorder = neworder (cachorder cache)
      , cached = newdict (cachorder cache)
      }
  where
    neworder (xs :|> x) = key <| xs
    newdict (xs :|> x) = M.delete x newcached
    filteredOrder = key <| Seq.filter (/= key) (cachorder cache)
    newcached = M.insert key val (cached cache)

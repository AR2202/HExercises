module Fizzbuzz
  ( fizzes
  , buzzes
  , fizzbuzzes
  , numfizzbuzz
  , fizzbuzz
  ) where

import           Data.Maybe

fizzes = cycle [Nothing, Nothing, Just "fizz"]

buzzes = cycle [Nothing, Nothing, Nothing, Nothing, Just "buzz"]

fizzbuzzes = zipWith (<>) fizzes buzzes

numbers = [1 ..]

numberstrings = map show numbers

numfizzbuzz = zipWith fromMaybe numberstrings fizzbuzzes

fizzbuzz n = mapM_ putStrLn $ take n numfizzbuzz

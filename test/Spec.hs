{-#LANGUAGE ScopedTypeVariables#-}
import Test.Hspec
import Test.QuickCheck
import Fizzbuzz
import Chapter23
main :: IO ()
main = hspec $ 
  describe "fizzes" $ do
    it "should have fizz as every third element" 
      $ property $
        \(NonNegative x::NonNegative Int)-> fizzes !! (3*x +2) == Just "fizz"
    describe "buzzes" $ do
      it "should have buzz as every fifth element"
        $ property $
        \(NonNegative x::NonNegative Int)-> buzzes !! (5*x +4) == Just "buzz"
      describe "fizzbuzzes" $ do
        it "should have fizzbuzz as every fifteenth element"
          $property$
          \(NonNegative x::NonNegative Int)-> fizzbuzzes !! (15*x +14) == Just "fizzbuzz"
          
        describe "numfizzbuzz" $ do
          it "should have Strings, not Maybe"
            $property$
            \(NonNegative x::NonNegative Int)-> numfizzbuzz !! (15*x +14) == "fizzbuzz"
           
          describe "numfizzbuzz" $ do
            it "should have number if it is not divisible by 3 or 5"
              $property$
              \(NonNegative x::NonNegative Int)-> x `mod`5>0 && x`mod`3 >0==> numfizzbuzz !! (x-1) == show x

            describe "neighbours" $ do
              it "should not contain the element itself"
                $property$
                \(coord::Coord)->coord `notElem` (neighbours coord)

              describe "neigbours" $ do
                it "should be symmetric"
                  $property$
                  \(coord::Coord)->forAll (choose (0, length (neighbours coord)-1)) $ \n -> coord `elem` ( neighbours $ (neighbours coord) !! n)
                
            




{-#LANGUAGE ScopedTypeVariables#-}
import Test.Hspec
import Test.QuickCheck
import Fizzbuzz
import Chapter23
main :: IO ()
main = hspec $ 
  describe "fizzes" $ do
    it "should have fizz as every third element" 
      $ property prop_fizzes_every_third
      
    describe "buzzes" $ do
      it "should have buzz as every fifth element"
        $ property prop_buzzes_every_fifth

      describe "fizzbuzzes" $ do
        it "should have fizzbuzz as every fifteenth element"
          $property prop_fizzbuzz_every_fifteenth

        describe "numfizzbuzz" $ do
          it "should have Strings, not Maybe"
            $property prop_numfizzbuzz_string
                    
          it "should have number if it is not divisible by 3 or 5"
            $property prop_numfizzbuzz_numb
             
          describe "neighbours" $ do
            it "should not contain the element itself"
              $property prop_neighbours_noSelf
             
            it "should be symmetric"
              $property prop_neighbours_symmetric

prop_fizzes_every_third :: NonNegative Int -> Bool
prop_fizzes_every_third (NonNegative x) = fizzes !! (3*x +2) == Just "fizz"

prop_buzzes_every_fifth :: NonNegative Int -> Bool
prop_buzzes_every_fifth (NonNegative x) = buzzes !! (5*x +4) == Just "buzz"

prop_fizzbuzz_every_fifteenth :: NonNegative Int-> Bool
prop_fizzbuzz_every_fifteenth (NonNegative x) = fizzbuzzes !! (15*x +14) == Just "fizzbuzz"

prop_numfizzbuzz_string :: NonNegative Int -> Bool
prop_numfizzbuzz_string (NonNegative x) = numfizzbuzz !! (15*x +14) == "fizzbuzz"       
            
prop_numfizzbuzz_numb:: NonNegative Int -> Property
prop_numfizzbuzz_numb (NonNegative x )= x `mod`5>0 && x`mod`3 >0==> numfizzbuzz !! (x-1) == show x

prop_neighbours_noSelf :: Coord -> Bool
prop_neighbours_noSelf coord = coord `notElem` (neighbours coord)


prop_neighbours_symmetric :: Coord -> Property
prop_neighbours_symmetric coord = forAll (choose (0, length (neighbours coord)-1)) $ \n -> coord `elem` ( neighbours $ (neighbours coord) !! n)
                

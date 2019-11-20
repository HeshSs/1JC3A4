{- Assignment 4 Tests
 - Name: Hishmat Salehi
 - Date: 11/17/2019
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing Test propDiff: "
          quickCheck propDiff
          print "Performing Test propSimp: "
          quickCheck propSimp
          print "Performing Test propValue: "
          quickCheck propValue

propValue :: (Floating a, Eq a, Ord a) => MathExpr a -> a -> Bool
propValue x y = (value (diff x) (y)) <= (value (x) (y))

propSimp :: (Floating a, Eq a, Ord a) => MathExpr a -> a -> Bool
propSimp x y = (value (simp x) y) == (value x y)

propDiff :: (Floating a, Eq a, Ord a) => MathExpr a -> Bool
propDiff x = simp (diff x) == simp (diff (simp x))
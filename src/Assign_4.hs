{- Assignment 4
 - Name: Hishmat Salehi
 - Date: 11/17/2019
 -}
module Assign_4 where

macid :: String
macid = "Salehh6"

data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: TODO add comments on value here
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value X n = n
value (Coef a) n = a
value (Sum a b) n = value a n + value b n
value (Prod a b) n = value a n * value b n
value (Quot a b) n = value a n / value b n
value (Exp a) n = exp (value a n)
value (Log a) n = log (value a n)

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: TODO add comments on simp here
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp X = X
simp (Coef a) = Coef a
-- Definition of simp for Sum
simp (Sum (Coef 0.0) u) = simp u
simp (Sum u (Coef 0.0)) = simp u
simp (Sum u v) = if u' == u && v' == v
                 then (Sum u v)
                    else simp (Sum u' v')
                        where
                            u' = simp u
                            v' = simp v
-- Definition of simp for Prod
simp (Prod (Coef 0.0) u) = Coef 0.0
simp (Prod u (Coef 0.0)) = Coef 0.0
simp (Prod (Coef 1.0) u) = simp u
simp (Prod u (Coef 1.0)) = simp u
simp (Prod u v) = if u' == u && v' == v
                  then (Prod u v)
                    else simp (Prod u' v')
                        where
                            u' = simp u
                            v' = simp v
-- Definition of simp for Quot
simp (Quot u (Coef 1.0)) = simp u
simp (Quot u v) = if u' == u && v' == v
                  then (Quot u v)
                    else simp (Quot u' v')
                        where
                            u' = simp u
                            v' = simp v
-- Definition of simp for Exp
simp (Exp (Coef 0.0)) = Coef 1.0
simp (Exp a) = Exp a
-- Definition of simp for Log
simp (Log (Coef 1.0)) = Coef 0.0
simp (Log a) = Log a

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: TODO add comments on diff here
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = (Coef 1.0)
diff (Coef a) = Coef 0.0
diff (Sum u v) = (Sum (diff u) (diff v))
diff (Prod u v) = (Sum (Prod (diff u) v) (Prod (diff v) u))
diff (Quot u v) = (Quot (Sum (Prod (diff u)  v) (Prod (Prod (Coef (-1)) u) (diff v))) (Prod v v)) -- ((diff u * v) + ((-1) * u * diff v)) / v * v
diff (Exp u) = (Prod (Exp u) (diff u))
diff (Log u) = (Quot (diff u) u)
{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: TODO add comments on readDiffWrite here
 -}

readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = do  x <- readFile f
                        let u = read x :: MathExpr Double
                            v = simp (diff u)
                        writeFile g (show v)
                    


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 1
 - - Input: value (Sum (Sum (Coef 2) X) X) 1
 - - Expected Output: 4.0
 - - Acutal Output: 4.0
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 2
 - - Input: value (Quot (Prod (Coef 2) X) X) 10
 - - Expected Output: 2.0
 - - Acutal Output: 2.0
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 3
 - - Input: value (Quot (Sum (Coef 100) (Prod (Coef 2) X)) X) 10
 - - Expected Output: 12.0
 - - Acutal Output: 12.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 1
 - - Input: simp (Quot (Sum (Exp (Coef 0)) (Prod (Coef 1) X)) X)
 - - Expected Output: Quot (Sum (Coef 1.0) X) X
 - - Acutal Output: Quot (Sum (Coef 1.0) X) X
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 2
 - - Input: simp (Quot (Sum (Log (Coef 1)) (Prod (Coef 1) X)) (Coef 1))
 - - Expected Output: X
 - - Acutal Output: X
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 3
 - - Input: simp (Quot (Sum (Coef 100) (Prod (Coef 2) X)) X)
 - - Expected Output: Quot (Sum (Coef 100.0) (Prod (Coef 2.0) X)) X
 - - Acutal Output: Quot (Sum (Coef 100.0) (Prod (Coef 2.0) X)) X
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 1
 - - Input: diff (simp (Quot (Sum (Log (Coef 1)) (Prod (Coef 1) X)) (Coef 1)))
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 2
 - - Input: diff (Quot (Sum (Coef 100) (Prod (Coef 2) X)) X)
 - - Expected Output: Quot (Sum (Prod (Sum (Coef 0.0) (Sum (Prod (Coef 0.0) X) (Prod (Coef 1.0) (Coef 2.0)))) X) (Prod (Prod (Coef (-1.0)) (Sum (Coef 100.0) (Prod (Coef 2.0) X))) (Coef 1.0))) (Prod X X)
 - - Acutal Output: Quot (Sum (Prod (Sum (Coef 0.0) (Sum (Prod (Coef 0.0) X) (Prod (Coef 1.0) (Coef 2.0)))) X) (Prod (Prod (Coef (-1.0)) (Sum (Coef 100.0) (Prod (Coef 2.0) X))) (Coef 1.0))) (Prod X X)
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 3
 - - Input: diff (Quot (Sum (Log (Coef 10)) (Prod X X)) (Coef 1))
 - - Expected Output: Quot (Sum (Prod (Sum (Quot (Coef 0.0) (Coef 10.0)) (Sum (Prod (Coef 1.0) X) (Prod (Coef 1.0) X))) (Coef 1.0)) (Prod (Prod (Coef (-1.0)) (Sum (Log (Coef 10.0)) (Prod X X))) (Coef 0.0))) (Prod (Coef 1.0) (Coef 1.0))
 - - Acutal Output: Quot (Sum (Prod (Sum (Quot (Coef 0.0) (Coef 10.0)) (Sum (Prod (Coef 1.0) X) (Prod (Coef 1.0) X))) (Coef 1.0)) (Prod (Prod (Coef (-1.0)) (Sum (Log (Coef 10.0)) (Prod X X))) (Coef 0.0))) (Prod (Coef 1.0) (Coef 1.0))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 1
 - - Input:  readDiffWrite "test.txt" "result.txt"
 - - Input File: (Quot (Sum (Log (Coef 10.0)) (Prod X X)) (Coef 1.0))
 - - Output File: Sum (Quot (Coef 0.0) (Coef 10.0)) (Sum X X)
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 2
 - - Input:  readDiffWrite "test.txt" "result1.txt"
 - - Input File: (Quot (Sum (Log (Coef 1)) (Prod (Coef 1) X)) (Coef 1))
 - - Output File: Coef 1.0
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 3
 - - Input:  readDiffWrite "test.txt" "result2.txt"
 - - Input File: Quot (Sum (Coef 100) (Prod (Coef 2) X)) X
 - - Output File: Quot (Sum (Prod (Coef 2.0) X) (Prod (Coef (-1.0)) (Sum (Coef 100.0) (Prod (Coef 2.0) X)))) (Prod X X)
 - -----------------------------------------------------------------
 - QuickCheck Test Cases
 - -----------------------------------------------------------------
 - - Function: value
 - - Property: 
        propValue :: (Floating a, Eq a, Ord a) => MathExpr a -> a -> Bool
        propValue x y = value (diff x) (y) <= value (x) (y)
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: simp
 - - Property: 
        propSimp :: (Floating a, Eq a, Ord a) => MathExpr a -> a -> Bool
        propSimp x y = (value (simp x) y) == (value x y)
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: diff
 - - Property: 
        propDiff :: (Floating a, Eq a, Ord a) => MathExpr a -> Bool
        propDiff x = simp (diff x) == simp (diff (simp x))
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 -
 -}

{- Assignment 4
 - Name: Hishmat Salehi
 - Date: 11/11/2019
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
-- Definition of simp for Prod
simp (Prod (Coef 0.0) u) = Coef 0.0
simp (Prod u (Coef 0.0)) = Coef 0.0
simp (Prod (Coef 1.0) u) = simp u
simp (Prod u (Coef 1.0)) = simp u
-- Definition of simp for Quot
simp (Quot u (Coef 1.0)) = simp u
-- Definition of simp for Exp
simp (Exp (Coef 0.0)) = Coef 1.0
-- Definition of simp for Log
simp (Log (Coef 1.0)) = Coef 0.0
-- General definition of simp
simp (Sum u v) = if u' == u && v' == v
                                then (Sum u v)
                                    else simp (Sum u' v')
                                    where
                                        u' = simp u
                                        v' = simp v
simp (Prod u v) = if u' == u && v' == v
                                then (Prod u v)
                                    else simp (Prod u' v')
                                    where
                                        u' = simp u
                                        v' = simp v
simp (Quot u v) = if u' == u && v' == v
                                then (Quot u v)
                                    else simp (Quot u' v')
                                    where
                                        u' = simp u
                                        v' = simp v

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: TODO add comments on diff here
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff u = error "TODO: implement diff"

{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: TODO add comments on readDiffWrite here
 -}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = error "TODO: implement readDiffWrite"

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function:
 - - Test Case Number:
 - - Input:
 - - Expected Output:
 - - Acutal Output:
 - -----------------------------------------------------------------
 - TODO: add test cases
 -}


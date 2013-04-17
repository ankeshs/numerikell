{-
	Licence Information

	This file is part of Numerikell 1.0.0.0 Haskell Numerical
	Software project. Please do not share, copy, edit or distribute
	without owner's permission.  

	@Contributors : Please striclty follow Haskell community convention.
	Comment your code, use proper nomenclature for functions, variables
	and modules.

	File Specification :
	Contributor : Mukul singh
-}
module Integrals (
  integrate
  )
  where

type Polynomial = [Float]

data Expr = Const Float | Poly Polynomial
	  | Sin Expr | Cos Expr | Tan Expr | Cosec Expr | Sec Expr | Cot Expr
	  | Log Expr | Exp Expr
	  | Sum Expr Expr | Product Expr Expr | Diff Expr Expr | Div Expr Expr
	  
	  
instance Show Expr where
  show (Const x) = show x
  show (Poly x) = show x
  show (Sin e) = "(sin $ " ++ show(e) ++ ")"
  show (Cos e) = "(cos $ " ++ show(e) ++ ")"
  show (Tan e) = "(tan $ " ++ show(e) ++ ")"
  show (Cosec e) = "(1/sin $ " ++ show(e) ++ ")"
  show (Sec e) = "(1/cos $ " ++ show(e) ++ ")"
  show (Cot e) = "(1/tan $ " ++ show(e) ++ ")"
  show (Log e) = "(log $ " ++ show(e) ++ ")"
  show (Exp e) = "(exp $ " ++ show(e) ++ ")"
  show (Sum t1 t2) = "(" ++ (show t1) ++ " + " ++ (show t2) ++ ")"
  show (Diff t1 t2) = "(" ++ (show t1) ++ " - " ++ (show t2) ++ ")"
  show (Product t1 t2) = "(" ++ (show t1) ++ " * " ++ (show t2) ++ ")"
  show (Div t1 t2) = "(" ++ (show t1) ++ " / " ++ (show t2) ++ ")"
  
--evaluates value of an expression at a specified point
eval :: Expr -> Float -> Float
eval (Const x) a = x
eval (Poly x) a = evalPoly x a
eval (Sin x) a = (sin (eval x a))
eval (Cos x) a = (cos (eval x a))
eval (Tan x) a = (tan (eval x a))
eval (Sec x) a = (1 / cos (eval x a))
eval (Cosec x) a = (1 / sin (eval x a))
eval (Cot x) a = (1 / tan (eval x a))
eval (Exp x) a = (exp (eval x a))
eval (Log x) a = (log (eval x a))
eval (Sum f g) a = (eval f a) + (eval g a)
eval (Diff f g) a = (eval f a) - (eval g a)
eval (Product f g) a =  (eval f a) * (eval g a)
eval (Div f g) a =  (eval f a) / (eval g a)

--evaluates value of a polynomial function at a specified point
evalPoly :: Polynomial -> Float -> Float
evalPoly [a] x = a
evalPoly a x = eval' (tail y) (head y) x
         where y = reverse a

-- helping function for eval
eval' :: Polynomial -> Float -> Float -> Float
eval' [a]    acc x = a + acc*x
eval' (a:as) acc x = eval' as (x*acc+a) x


--takes an expression with lower and upper limits, and gives the integration of the expression from lower limit to upper limit
integrate :: Expr -> Float -> Float -> Float
integrate expr a0 a1 = if (a0==a1) 
			  then 0 
			  else integrate' expr a0 ((a1-a0)/100.0) 1

--helping fucntion for integrate			  
integrate' :: Expr -> Float -> Float -> Float -> Float
integrate' expr a0 h n = 
  if n==100
     then (eval expr (a0 + (n-1) * h) ) * h
     else (eval expr (a0 + (n-1) * h) ) * h + (integrate' expr a0 h (n+1) )
  
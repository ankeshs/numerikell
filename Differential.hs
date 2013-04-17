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
module Differential(
  diff,
  deriv
  )
  where
import Data.String.Utils


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
  
  
-- takes an expression and evaluates its value at a specified point
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


-- evaluate a polynomial function at a specified point
evalPoly :: Polynomial -> Float -> Float
evalPoly [a] x = a
evalPoly a x = eval' (tail y) (head y) x
         where y = reverse a

-- helping function for eval
eval' :: Polynomial -> Float -> Float -> Float
eval' [a]    acc x = a + acc*x
eval' (a:as) acc x = eval' as (x*acc+a) x


--takes an expression and gives its derivative at a specified point
diff :: Expr -> Float -> Float
diff (Const x) a = 0
diff (Poly x) a = evalPoly (diffPoly x) a
diff (Sin x) a = (eval (Cos x) a) * (diff x a)
diff (Cos x) a = (eval (Const (-1)) a) * (eval (Sin x) a) * (diff x a)
diff (Tan x) a = (eval (Sec x) a) * (eval (Sec x) a) * (diff x a)
diff (Sec x) a = (eval (Sec x) a) * (eval (Tan x) a) * (diff x a)
diff (Cosec x) a = (eval (Const (-1)) a) * (eval (Cosec x) a) * (eval (Cot x) a) * (diff x a)
diff (Cot x) a = (eval (Const (-1)) a) * (eval (Cosec x) a) * (eval (Cosec x) a) * (diff x a)
diff (Exp x) a = (eval (Exp x) a) * (diff x a)
diff (Log x) a = (eval (Log x) a) * (diff x a)
diff (Sum f g) a = sumRule f g a
diff (Diff f g) a = diffrenceRule f g a
diff (Product f g) a = productRule f g a 
diff (Div f g) a = divRule f g a 

--implements sum rule of differentiation (f+g)' = f' + g'
sumRule :: Expr -> Expr -> Float -> Float
sumRule f g a = (diff f a) + (diff g a)

--implements difference rule of differentiation (f-g)' = f' - g'
diffrenceRule :: Expr -> Expr -> Float -> Float
diffrenceRule f g a = (diff f a) - (diff g a)

--implements product rule of differentiation (f*g)' = f*g' + g*f'
productRule :: Expr -> Expr -> Float -> Float
productRule f g a = (eval f a) * (diff g a) + (eval g a) * (diff f a)

--implements division rule of differentiation (f/g)' = (f'g - g'f)/g^2
divRule :: Expr -> Expr -> Float -> Float
divRule f g a = ((diff f a) * (eval g a) - (diff g a) * (eval f a)) / ((eval g a)* (eval g a))

-- to find the derivative of a polynomial function
diffPoly :: Polynomial -> Polynomial
diffPoly a = diffPoly' (tail a) 1

-- helping function for diffPoly
diffPoly' :: Polynomial -> Float -> Polynomial
diffPoly' [x] i = [x*i]
diffPoly' (x:xs) i = (x*i) : (diffPoly' xs (i+1))

--takes an expression and gives its derivative as an expression string
deriv :: Expr -> String
deriv e = (replace "\"" "" str)
  where str = deriv' e


--helping function for deriv
deriv' :: Expr -> String
deriv' (Const _) = show(Const 0)
deriv' (Poly x) = show(diffPoly x)
deriv' (Sin x) = show(Cos x) ++ " * " ++ (deriv' x)
deriv' (Cos x) = show(Const (-1)) ++ " * " ++ show(Sin x) ++ " * " ++ (deriv' x)
deriv' (Tan x) = show(Sec x) ++ " * " ++ show(Sec x) ++ " * " ++ (deriv' x)
deriv' (Sec x) = show(Sec x) ++ " * " ++ show(Tan x) ++ " * " ++ (deriv' x)
deriv' (Cosec x) = show(Const (-1)) ++ " * " ++ show(Cosec x) ++ " * " ++ show(Cot x) ++ " * " ++ (deriv' x)
deriv' (Cot x) = show(Const (-1)) ++ " * " ++ show(Cosec x) ++ " * " ++ show(Cosec x) ++ " * " ++ (deriv' x)
deriv' (Exp x) = show(Exp x) ++ " * " ++ (deriv' x) 
deriv' (Log x) = "1/(" ++ show(x) ++ ") * " ++ (deriv' x) 
deriv' (Sum f g) = "(" ++ (deriv' f) ++ " + " ++ (deriv' g) ++ ")"
deriv' (Product f g) = "(" ++ show(f) ++ " * " ++ (deriv' g) ++ " + " ++ (deriv' f) ++ " * " ++ show(g) ++ ")"


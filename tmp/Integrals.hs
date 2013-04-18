-- | module Integrals provides the functionality to find the definite integral of some standard functions
-- functions implemented currently are constant, polynomial, sin, cos, tan, cosec, sec, cot, log, exponential and their compositions.
-- Also, the functions formed by addition, subtraction, product and division of these functions are allowed
module Integrals (
  -- * Types
  -- ** Polynomial type
  Polynomial,
  -- * Data Types
  -- ** Expr 
  Expr(Const,Poly,Sin,Cos,Tan,Cosec,Sec,Cot,Log,Exp,Sum,Product,Diff,Div),
  -- * Funtions
  integrate
  )
  where

type Polynomial = [Float] -- ^ List of float elements [1,3,2] means 1 + 3x + 2x^2

-- |Expr data type contains expressions for the mathematical expressions
data Expr 
  -- | To define an expression as a constant float number
  = Const Float
  -- | To define an expression as a polynomial
  | Poly Polynomial
  -- | To define an expression as Sin function
  | Sin Expr 
  -- | To define an expression as Cos function
  | Cos Expr 
  -- | To define an expression as Tan function
  | Tan Expr 
  -- | To define an expression as Cosec function
  | Cosec Expr 
  -- | To define an expression as Sec function
  | Sec Expr 
  -- | To define an expression as Cot function
  | Cot Expr
  -- | To define an expression as Log function
  | Log Expr 
  -- | To define an expression as Exponential function
  | Exp Expr
  -- | To define an expression as a sum of two expressions
  | Sum Expr Expr 
  -- | To define an expression as a product of two expressions
  | Product Expr Expr 
  -- | To define an expression as a difference of two expressions
  | Diff Expr Expr 
  -- | To define an expression as a divison of two expressions
  | Div Expr Expr	  
	  
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


evalPoly :: Polynomial -> Float -> Float
evalPoly [a] x = a
evalPoly a x = eval' (tail y) (head y) x
         where y = reverse a

-- helping function for eval
eval' :: Polynomial -> Float -> Float -> Float
eval' [a]    acc x = a + acc*x
eval' (a:as) acc x = eval' as (x*acc+a) x


-- | The integrate function finds the definite integral of the functions, using trapezoid method
integrate :: Expr 	-- ^ Input Expression
  -> Float 		-- ^ lower limit of integration
  -> Float 		-- ^ upper limit of integration
  -> Float		-- ^ return value of the integration
integrate expr a0 a1 = if (a0==a1) 
			  then 0 
			  else integrate' expr a0 ((a1-a0)/100.0) 1


integrate' :: Expr -> Float -> Float -> Float -> Float
integrate' expr a0 h n = 
  if n==100
     then (eval expr (a0 + (n-1) * h) ) * h
     else (eval expr (a0 + (n-1) * h) ) * h + (integrate' expr a0 h (n+1) )
  
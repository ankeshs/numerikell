-- | module Equation provides functions for solving one dimentional equations in polynomial functions
-- It also provides other functions related to polynomials like degree, polynomial evaluation etc.
-- This module also contains functions for addition, subtraction and multiplication of polynomials
-- It also finds the roots of polynomials using Newton Raphson method.
module Equation (
  -- * Types
  -- ** Polynomial type
  -- *** List of Double elements, [1,3,2] means 1 + 3x + 2x^2
  Polynomial,
  -- * Funtions
  eval,
  deg,
  root1,
  root2,
  newRaph,
  scalar,
  add,
  sub,
  mul,
  update,
  quotL,
  newRaph1,
  )
  where

type Polynomial = [Double]

-- |evaluate a polynomial at a particular value
eval :: Polynomial -> Double -> Double
eval [a] x = a
eval a x = eval' (tail y) (head y) x
         where y = reverse a

-- helping function for eval
eval' :: Polynomial -> Double -> Double -> Double
eval' [a]    acc x = a + acc*x
eval' (a:as) acc x = eval' as (x*acc+a) x

-- | returns the degree of a polynomial
deg :: Polynomial -> Int
deg x = length x -1

-- |root of linear equation
root1 :: Polynomial -> Double
root1 [a0,a1] = (-a0/a1)

-- |root of quadratic equation
root2 :: Polynomial -> [Double]
root2 [c,b,a] =  [(-b + sqrt (b*b-4*a*c))/(2*a),(-b - sqrt (b*b-4*a*c))/(2*a)]

-- derivative of a polynomial
deriv :: Polynomial -> Polynomial
deriv a = deriv' (tail a) 1

-- helping function for deriv
deriv' :: Polynomial -> Double -> Polynomial
deriv' [x] i = [x*i]
deriv' (x:xs) i = (x*i) : (deriv' xs (i+1))

-- | Newton-Raphson algorithm for finding roots
newRaph :: Polynomial -> Double
newRaph a = newRaph' a (deriv a) 0

-- helping function for newRaph
newRaph' :: Polynomial -> Polynomial -> Double -> Double
newRaph'  a da x = if (abs (eval a x) < 0.01)
                   then x
                   else newRaph' a da (x - (eval a x)/(eval da x))

-- |scalar multiplciation with a polynomial
scalar :: Polynomial -> Double -> Polynomial
scalar xs a = map (\t-> a*t) xs



-- |add polynomial
add :: Polynomial -> Polynomial -> Polynomial
add []      ys    = ys
add xs      []    = xs
add (x:xs) (y:ys) = (x+y) : add xs ys

-- |subtract polynomial
sub :: Polynomial -> Polynomial -> Polynomial
sub []      []    = []
sub []      ys    = scalar ys (-1)
sub xs      []    = xs
sub (x:xs) (y:ys) = (x-y) : sub xs ys

-- |mutliply polynomial
mul :: Polynomial -> Polynomial -> Polynomial
mul [x] y = scalar y x
mul (x:xs) y = add (scalar y x) (rshift (mul xs y))

-- right shift the coefficient vector. Eq: P -> xP
rshift :: Polynomial -> Polynomial
rshift [] = []
rshift (x:xs) = 0:x:xs

-- left shift the coefficient vector. P -> P/x assuming P(0) = 0
lshift :: Polynomial -> Polynomial
lshift [] = []
lshift (0:xs) = xs

-- |update a coefficient of a polynomial
update :: Polynomial -> Int -> Double -> Polynomial
update x i val = f ++ [val] ++ tail g
                where y = splitAt i x
                      f = fst y
                      g = snd y

{-|
divide a polynomial P(x) by (x-a) when P(a)=0. Return quotient

  a0 + a1x a2x2 + ... anxn = (x-a)(b0 + b1x +b2x2 + .... bn-1 xn-1)
                           = - ab0 + (b0-b1a)x + (b1-ab2)x2 + b2x3 .......(bn-2-abn-1)xn-1 +  bn-1 xn
                       b0 = -a0/a
                       b1 = (b0 -a1)/a
                       bi = (bi-1 - ai)/a     0<i<=n-1
                       bn-1 = (bn-2 - an-1)/a
                       special case for a=0
-}
quotL :: Polynomial -> Double -> Polynomial
quotL  c 0  = lshift c
quotL  c x  = quotL' c x 0


-- helping function for quotL
quotL' :: Polynomial -> Double -> Int -> Polynomial
quotL' (c:cs) x 0 = quotL' ((-c/x):cs) x 1
quotL' c x i = if (i == (length c)-1)
                then init c
                else quotL' (update c i ((c !! (i-1) - c !! i)/x) ) x (i+1)

-- |repeated Newton Raphson Algorithm
newRaph1 :: Polynomial -> [Double]
newRaph1 c   = if (length c <= 1)
                then []
                else root : (newRaph1 (quotL c root))
                where root = newRaph c


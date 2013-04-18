-- | This module facilitates the use of vectors by providing various functionalities of vector calculus
module Vector (
  -- * types
  Vector,
  -- * functions
  dot,
  sub,
  add,
  app,
  size,
  i,
  j,
  k,
  cross,
  norm,
  unity
  )where

type Vector = [Double] -- ^ List of Double elements

-- |dot product of two vectors
dot :: Vector -> Vector -> Double
dot [] _ = 0
dot _ [] = 0
dot (x:xs) (y:ys) = x*y + dot xs ys

-- |return difference of two vector
sub :: Vector -> Vector -> Vector
sub [] (x:xs) = map (\x -> -x) (x:xs)
sub (x:xs) [] =  (x:xs) 
sub (x:xs) (y:ys) = (x-y) : sub xs ys      

-- |return addition of two vector
add :: Vector -> Vector -> Vector
add [] (x:xs) = (x:xs)	
add (x:xs) [] = (x:xs)	
add (x:xs) (y:ys) = (x+y) : add xs ys


-- |apply a function, component wise on two vectors
app :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
app _ [] _ = []
app _ _ [] = []
app f (x:xs) (y:ys) = (f x y) : (app f xs ys)


-- |size of a vector
size :: Vector -> Int
size  = foldl (\acc  t-> acc+1) 0

-- |return vector i
i :: Vector
i  = [1,0,0]

-- |return vector j
j :: Vector
j  = [0,1,0]

-- |return vector k
k :: Vector
k  = [0,0,1]

-- |cross product of two vectors
cross :: Vector -> Vector -> Vector
cross [x1,x2,x3] [y1,y2,y3] = [x2*y3-x3*y2, x3*y1-x1*y3, x1*y2-x2*y1]

-- |normalized distance of a vector
norm :: Vector-> Double
norm = sqrt. foldl (\acc t -> acc + t*t) 0

-- |find a unit vector along the vector
unity :: Vector -> Vector
unity x = map (\t -> t/normalized) x
        where  normalized = norm x

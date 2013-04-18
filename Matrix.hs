-- |This modules provides functions commonly used for working with matrices
module Matrix (
  -- * funtions
  size,
  zero,
  rowMajor,
  colMajor,
  min2D,
  size2D,
  valid2D,
  minCol,
  maxCol,
  det,
  removeColI,
  removeRowI,
  transpose,
  coli,
  rowi,
  mul,
  appOnly,
  scalar,
  add,
  sub,
  app,
  foldl2D,
  foldlInt,
  foldInit,
  foldlAll,
  foldr2D,
  minor,
  cofactor,
  cofactorM,
  minorM,
  adjugate,
  inverse
  )
  where


-- |takes a list and returns the number of elements in the list
size :: [a] -> Int
size = length

-- |return a 0 matrix of size rxc
zero :: (Fractional a) => Int -> Int -> [[a]]
zero row col = replicate row (replicate col 0)

-- |take a matrix and return the row-major list
rowMajor :: [[a]] -> [a]
rowMajor [] = []
rowMajor (x:xs) = x ++ (rowMajor xs)

-- |take a matrix and return the column-major list
colMajor :: [[a]] -> [a]
colMajor = rowMajor.transpose

-- |take a matrix and return the minimum element
min2D :: (Ord a) => [[a]] -> a
min2D [x]  = foldl (\t acc -> min t acc) (x!!0) x
min2D (x:xs) = min (foldl (\acc t -> min t acc) (x!!0) x) (min2D xs) 

-- |takes a 2D matrix in row-major form and returns row and maximum column
size2D :: [[a]] -> (Int,Int)
size2D [] = (0,0)
size2D (x:xs) = (1+row,max (size x) col)
		where   rem  = size2D xs
			row  = fst rem
			col  = snd rem	

-- |checks whether the given row major form has same number of columns
valid2D :: [[a]] -> Bool
valid2D x = (minCol x) == (maxCol x)

-- |returns minimum column in a row-major 2D matrix
minCol :: [[a]] -> Int
minCol [x] = size x
minCol [] = 0
minCol (x:xs) = min (size x) (minCol xs)

-- |returns maximum column in a row-major 2D matrix
maxCol :: [[a]] -> Int
maxCol [] = 0
maxCol (x:xs) = max (size x) (maxCol xs)

-- |computes determinant of a row-major array
det :: (Fractional a) => [[a]] -> a
det [] = 0
det (x:xs) = deti x xs 0

--helping function for det
deti :: (Fractional a) => [a] -> [[a]] -> Int -> a
deti x  []  _              =  x !! 0
deti x  xs i 
	| i <  (size x)-1  = (-1)^i*(x !! i)* det (removeColI xs i) +  deti x xs (i+1)
	| i == (size x)-1  = (-1)^i*(x !! i)* det (removeColI xs i)


-- |removes ith column of a 2D matrix
removeColI :: [[a]] -> Int -> [[a]]
removeColI []     _ = []
removeColI (x:xs) i = (rmvFrmLst x i 0):(removeColI xs i)

-- |remove kth row of a 2D matrix
removeRowI :: [[a]] -> Int -> [[a]]
removeRowI x t = removeRowI' x t 0

-- help function for remove ith row of a 2D matrix
removeRowI' :: [[a]] -> Int -> Int -> [[a]]
removeRowI' []     t i = []
removeRowI' (x:xs) t i = if (i==t)
                         then xs
                         else x:(removeRowI' xs t (i+1))  


-- removes ith element from a list
rmvFrmLst :: [a] -> Int -> Int -> [a]
rmvFrmLst [] _ _ = []
rmvFrmLst (x:xs) i iter = if (i==iter)
			then    rmvFrmLst xs i (iter+1)
			else x:(rmvFrmLst xs i (iter+1))


-- |transpose a given matrix
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose x = y:(transpose z)
        where y = map (\t -> head t) x
              z = filter (\u-> size u /= 0) $ map (\t -> tail t) x

-- dot product of two list
dot :: (Num a) => [a] -> [a] -> a
dot [] _ = 0
dot _  [] = 0
dot (x:xs) (y:ys) = x*y + (dot xs  ys)

-- |extract ith column 0 being the 1st column
coli :: [[a]] -> Int -> [a]
coli [] _ = []
coli x i  = if (i==0)
                then y
                else coli z (i-1)
            where y = map (\t -> head t) x
                  z = filter (\u-> size u /= 0) $ map (\t -> tail t) x

-- |extract ith row 0 being the 1st row
rowi :: [[a]] -> Int -> [a]
rowi [] _     = []
rowi (x:xs) i = if (i==0)
                then x
                else rowi xs(i-1)

                                                
-- |multiply two matrices
mul :: (Num a) => [[a]] -> [[a]] -> [[a]]
mul []    _  = []
mul _     [] = []
mul (x:xs) y = (mult' x y 0) : (mul xs y)

-- support function for mult
mult' :: (Num a) => [a] -> [[a]] -> Int -> [a]
mult' x y i =   if (i< (snd(size2D y)))
                then (dot x (coli y i)):(mult' x y (i+1))
                else []

-- |apply a function to all entries of the matrix
appOnly :: (a->b) -> [[a]] -> [[b]]
appOnly f x = map (\t -> map f t) x

-- |scalar multiplication
scalar :: (Fractional a) => [[a]] -> a -> [[a]]
scalar x c = appOnly (\t->t*c) x


-- |add two matrices
add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add = app (+)

-- |subtract two matrices
sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub = app (-)

-- |apply a function on two matrices
app :: (Num a) => (a->b->c) -> [[a]] -> [[b]] -> [[c]]
app _   []     _      =  []
app _   _      []     =  []
app f (  x:xs) (y:ys) = ( map (\(u,v) -> f u v) $ zip x y): (app f xs ys)

-- |fold a list of 2D matrices from left
foldl2D :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[b]]
foldl2D _ [] acc = acc
foldl2D f (x:xs) acc = foldl2D f xs (f x acc)

-- |fold a list of 2D matrices from left and store intermediate values
foldlInt :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[[b]]]
foldlInt _ [] acc = [acc]
foldlInt f (x:xs) acc = acc:(foldlInt f xs (f x acc))

-- |fold a list of 2D matrices from left with starting value 0 matrix
foldInit :: (Fractional a) => ([[a]]->[[a]]->[[a]]) -> [[[a]]] -> [[a]]
foldInit f x = foldl2D f x (zero row col)
                where row = fst dim
                      col = snd dim
                      dim = size2D x

-- |fold a list of 2D matrices from left, with starting value 0 and storing intermediate  values
foldlAll :: (Fractional a) => ([[a]]->[[a]]->[[a]]) -> [[[a]]] -> [[[a]]]
foldlAll f x = foldlInt f x (zero row col)
                where row = fst dim
                      col = snd dim
                      dim = size2D x

-- |fold a list of 2D matrices from right
foldr2D :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[b]]
foldr2D f  x acc = foldl2D f (reverse x) acc

-- |returns Minor Mij
minor :: (Fractional a) => [[a]] -> Int -> Int -> a
minor x row col = det (removeRowI (removeColI x col) row)

-- |cofactor Cij
cofactor :: (Fractional a) => [[a]] -> Int -> Int -> a
cofactor x row col =  (-1)^(row+col)* minor x row col 

-- |returns cofactor matrix
cofactorM :: (Fractional a) => [[a]] -> [[a]]
cofactorM x = map (\r -> (map (\c -> cofactor x r c) cs)) rs
         where rs   = [u | u <- [0..(row-1)]]
               cs   = [v | v <- [0..(col-1)]]
               row = length x
               col = minCol x

-- |returns minor matrix
minorM :: (Fractional a) => [[a]] -> [[a]]
minorM x = map (\r -> (map (\c -> minor x r c) cs)) rs
         where rs   = [u | u <- [0..(row-1)]]
               cs   = [v | v <- [0..(col-1)]]
               row = length x
               col = minCol x
-- |adjugate matrix
adjugate :: (Fractional a) => [[a]] -> [[a]]
adjugate  = transpose.cofactorM

-- |inverse of a matrix
inverse :: (Fractional a, Eq a) => [[a]] -> Either String [[a]]
inverse x = if (detValue == 0)
            then Left "Matrix is Singular"
            else Right (scalar (adjugate x) (1/detValue))
            where detValue = det x
  



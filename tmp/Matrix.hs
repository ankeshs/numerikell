{-|
	Licence Information

	This file is part of Numerikell 1.0.0.0 Haskell Numerical
	Software project. Please do not share, copy, edit or distribute
	without owner's permission.  

	@Contributors : Please striclty follow Haskell community convention.
	Comment your code, use proper nomenclature for functions, variables
	and modules.

	File Specification :
	Contributor : Dipendra K. Misra (dipendrakumarmisra@gmail.com), Mukul Singh
-}
module Matrix where


-- takes a list and returns the number of elements in the list
size :: [a] -> Int
size = length

-- return a 0 matrix of size rxc
zero :: Int -> Int -> [[Int]]
zero row col = replicate row (replicate col 0)

-- take a matrix and return the row-major list
rowMajor :: [[a]] -> [a]
rowMajor [] = []
rowMajor (x:xs) = x ++ (rowMajor xs)

-- take a matrix and return the column-major list
colMajor :: [[a]] -> [a]
colMajor = rowMajor.transpose

-- take a matrix and return the minimum element
min2D :: (Ord a) => [[a]] -> a
min2D [x]  = foldl (\t acc -> min t acc) (x!!0) x
min2D (x:xs) = min (foldl (\acc t -> min t acc) (x!!0) x) (min2D xs) 

-- takes a 2D matrix in row-major form and returns row and maximum column
size2D :: [[a]] -> (Int,Int)
size2D [] = (0,0)
size2D (x:xs) = (1+row,max (size x) col)
		where   rem  = size2D xs
			row  = fst rem
			col  = snd rem	

-- checks whether the given row major form has same number of columns
valid2D :: [[a]] -> Bool
valid2D x = (minCol x) == (maxCol x)

-- returns minimum column in a row-major 2D matrix
minCol :: [[a]] -> Int
minCol [x] = size x
minCol [] = 0
minCol (x:xs) = min (size x) (minCol xs)

-- returns maximum column in a row-major 2D matrix
maxCol :: [[a]] -> Int
maxCol [] = 0
maxCol (x:xs) = max (size x) (maxCol xs)

-- computes determinant of a row-major array
det :: [[Int]] -> Int
det [] = 0
det (x:xs) = deti x xs 0

deti :: [Int] -> [[Int]] -> Int -> Int
deti x  []  _              =  x !! 0
deti x  xs i 
	| i <  (size x)-1  = (-1)^i*(x !! i)* det (removeColI xs i) +  deti x xs (i+1)
	| i == (size x)-1  = (-1)^i*(x !! i)* det (removeColI xs i)


-- removes ith column of a 2D matrix
removeColI :: [[a]] -> Int -> [[a]]
removeColI []     _ = []
removeColI (x:xs) i = (rmvFrmLst x i 0):(removeColI xs i)


-- removes ith element from a list
rmvFrmLst :: [a] -> Int -> Int -> [a]
rmvFrmLst [] _ _ = []
rmvFrmLst (x:xs) i iter = if (i==iter)
			then    rmvFrmLst xs i (iter+1)
			else x:(rmvFrmLst xs i (iter+1))


-- transpose a given matrix
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

-- extract ith column 0 being the 1st column
coli :: [[a]] -> Int -> [a]
coli [] _ = []
coli x i  = if (i==0)
                then y
                else coli z (i-1)
            where y = map (\t -> head t) x
                  z = filter (\u-> size u /= 0) $ map (\t -> tail t) x

-- extract ith row 0 being the 1st row
rowi :: [[a]] -> Int -> [a]
rowi [] _     = []
rowi (x:xs) i = if (i==0)
                then x
                else rowi xs(i-1)

                                                
-- multiply two matrices
mul :: (Num a) => [[a]] -> [[a]] -> [[a]]
mul []    _  = []
mul _     [] = []
mul (x:xs) y = (mult' x y 0) : (mul xs y)

-- support function for mult
mult' :: (Num a) => [a] -> [[a]] -> Int -> [a]
mult' x y i =   if (i< (snd(size2D y)))
                then (dot x (coli y i)):(mult' x y (i+1))
                else []

-- add two matrices
add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add = app (+)

-- subtract two matrices
sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub = app (-)

-- apply a function on two matrices
app :: (Num a) => (a->b->c) -> [[a]] -> [[b]] -> [[c]]
app _   []     _      =  []
app _   _      []     =  []
app f (  x:xs) (y:ys) = ( map (\(u,v) -> f u v) $ zip x y): (app f xs ys)

-- fold a list of 2D matrices from left
foldl2D :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[b]]
foldl2D _ [] acc = acc
foldl2D f (x:xs) acc = foldl2D f xs (f x acc)

-- fold a list of 2D matrices from left and store intermediate values
foldlInt :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[[b]]]
foldlInt _ [] acc = [acc]
foldlInt f (x:xs) acc = acc:(foldlInt f xs (f x acc))

-- fold a list of 2D matrices from left with starting value 0 matrix
foldInit :: ([[a]]->[[Int]]->[[Int]]) -> [[[a]]] -> [[Int]]
foldInit f x = foldl2D f x (zero row col)
                where row = fst dim
                      col = snd dim
                      dim = size2D x

-- fold a list of 2D matrices from left, with starting value 0 and storing intermediate  values
foldlAll :: ([[a]]->[[Int]]->[[Int]]) -> [[[a]]] -> [[[Int]]]
foldlAll f x = foldlInt f x (zero row col)
                where row = fst dim
                      col = snd dim
                      dim = size2D x

-- fold a list of 2D matrices from right
foldr2D :: ([[a]]->[[b]]->[[b]]) -> [[[a]]] -> [[b]] -> [[b]]
foldr2D f  x acc = foldl2D f (reverse x) acc



  



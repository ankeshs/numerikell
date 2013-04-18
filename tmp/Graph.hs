-- | module Graph provides functions commonly used in graph theory
module Graph (
  -- * Types
  -- ** Graph 
  Graph,
  -- * Data Types
  -- ** BinaryTree
  BinaryTree(Empty,Node),
  -- ** Tree 
  Tree(NIL,Key),
  -- * Funtions
  rectify,
  applicator,
  consistent,
  consistent1,
  v,
  v1,
  isUndirected,
  maxOutdegree,
  minOutdegree,
  complement,
  applicator2,
  union,
  subtraction,
  inOrder,
  preOrder,
  postOrder,
  preOrderTree,
  postOrderTree,
  insert,
  insertL,
  delete,
  deleteL,
  leftMost,
  removeLeft,
  rightMost,
  removeRight,
  )
  where

type Graph = [[Int]] -- ^ List of List of Int


-- |BinaryTree data type is a constructor to define binary tree
data BinaryTree a = Empty -- ^ defines empty tree
		  | Node a (BinaryTree a) (BinaryTree a) 	-- ^ defines a tree node which has left and right children
		  deriving (Show) 		

-- |Tree data type is a constructor to define a generic tree
data Tree a = NIL -- ^ Empty tree
	    | Key a [Tree a] 	-- ^ defines a node with a particular value and its children as a List of 'Tree'
	    deriving (Show)	

-- |rectifies a graph makes all left diagonal entries as 1
rectify :: Graph -> Graph
rectify g = applicator g (\_ -> 1) (\i j -> i==j)

-- |applies a function to all edge entries which satisfy a predicate
applicator :: Graph -> (Int-> Int) -> (Int->Int->Bool) -> Graph
applicator g f p = map (\row ->  map (\col -> if (p row col) then (f (g !! row !! col)) else g !! row !! col) i) i
                 where n = v1 g
                       i = [ u | u <- [0..(n-1)]]

-- |evaluate whether a graph is consistent, returns maybe datatype
consistent :: Graph -> Either String String
consistent x = if length x == maximum (map (\t -> length t) x)
               then Right ("Graph is Consistent with number of vertices as "++show(length x))
               else Left "Graph is Not Consistent"

-- |evaluate whether a graph is consistent, returns true or false
consistent1 :: Graph -> Bool
consistent1 x = if length x == maximum (map (\t -> length t) x)
               then True
               else False

-- |Number of vertices, returns maybe datatype
v :: Graph -> Either String Int
v x = if (consistent1 x)
      then Right (length x)
      else Left "InConsistent Value"

-- |Number of vertices, returns int
v1 :: Graph -> Int
v1 x = if (consistent1 x)
      then length x
      else 0

-- |is undirected graph
isUndirected :: Graph -> Bool
isUndirected x = and (map (\(a,b) -> (x !! a !! b) == (x !! b !! a)) [ (u,v) | u <- [0..(n-1)], v <- [0..(n-1)] ])
                 where n =  v1 x

-- |maximum out-degree
maxOutdegree :: Graph -> Int
maxOutdegree x = maximum (map (\t -> foldl (\acc v -> if (v>0) then acc+1 else acc) 0 t) x) -1

-- |min out-degree
minOutdegree :: Graph -> Int
minOutdegree x = minimum (map (\t -> foldl (\acc v -> if (v>0) then acc+1 else acc) 0 t) x) -1

-- |complement graph
complement :: Graph -> Graph
complement g = applicator g (\val -> mod (val+1) 2 ) (\i j -> i/=j)

-- |apply a function on values of graph edge (of same dimension) entries when a predicate is satisfied
applicator2 :: Graph -> Graph -> (Int-> Int->Int) -> (Int->Int->Bool) -> Graph
applicator2 g1 g2 f p = map (\row ->  map (\col -> if (p row col) then (f (g1 !! row !! col) (g2 !! row !! col)) else g1 !! row !! col) i) i
                 where n = v1 g1
                       i = [ u | u <- [0..(n-1)]]

-- |graph union
union :: Graph -> Graph -> Graph
union g1 g2 = applicator2 g1 g2 (\u v -> if(u==0 && v==0) then 0 else 1) (\_ _ -> True)

-- |graph subtraction
subtraction :: Graph -> Graph -> Graph
subtraction g1 g2 = applicator2 g1 g2 (\u v -> if(u==1 && v==1) then 0 else u) (\_ _ -> True)

-- |binary tree in-order traversal
inOrder :: (Ord a) => BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x y z) = (inOrder y)++[x]++(inOrder z)

-- |binary tree pre-order traversal
preOrder :: (Ord a) => BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node x y z) = [x]++(preOrder y)++(preOrder z)

-- |binary tree post-order traversal
postOrder :: (Ord a) => BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node x y z) = (postOrder y)++(postOrder z)++[x]

-- |pre-order Tree Traversal
preOrderTree :: (Ord a) => Tree a -> [a]
preOrderTree NIL = []
preOrderTree (Key a x) = [a]++(concat (map (\t -> preOrderTree t) x))

-- |pre-order Tree Traversal
postOrderTree :: (Ord a) => Tree a -> [a]
postOrderTree NIL = []
postOrderTree (Key a x) = [a]++(concat (map (\t -> postOrderTree t) x))

-- |insert into binary tree
insert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insert Empty x = Node x (Empty) (Empty)
insert (Node v l r) x = if (v >= x )
                        then (Node v (insert l x) r)
                        else (Node v l (insert r x))

-- |insert a list into a tree
insertL :: (Ord a) => BinaryTree a -> [a] -> BinaryTree a
insertL  = foldl (\acc v -> insert acc v)

-- |delete from trees
deleteL :: (Ord a) => BinaryTree a -> [a] -> BinaryTree a
deleteL = foldl (\acc v -> delete acc v)              

-- |delete from a binary tree
delete :: (Ord a) => BinaryTree a -> a -> BinaryTree a
delete Empty x = Empty
delete (Node v l Empty) x = if (v==x)
		            then l 
			    else if (v > x)
			         then (Node v (delete l x) Empty)
			         else (Node v l Empty)
delete (Node v l r)    x  = if (v==x)
                            then (Node z l r)
                            else if (v > x)
                                then (Node v (delete l x) r)
                                else (Node v l (delete r x))
                           where Right z = leftMost r			      



-- |finds leftmost node of a tree
leftMost :: (Ord a) => BinaryTree a -> Either String a
leftMost Empty             = Left "None Exist"
leftMost (Node v Empty r ) = Right v
leftMost (Node v l r)      = leftMost l

-- |removes the leftmost node of the tree if there is one
removeLeft :: (Ord a) => BinaryTree a -> BinaryTree a
removeLeft Empty = Empty
removeLeft (Node v Empty r) = r
removeLeft (Node v l r )    = (Node v (removeLeft l) r)

-- |finds rightmost node of a tree
rightMost :: (Ord a) => BinaryTree a -> Either String a
rightMost Empty             = Left "None Exist"
rightMost (Node v l Empty)  = Right v
rightMost (Node v l r)      = rightMost r 

-- |removes the leftmost node of the tree if there is one
removeRight :: (Ord a) => BinaryTree a -> BinaryTree a
removeRight Empty = Empty
removeRight (Node v l Empty) = l
removeRight (Node v l r )    = (Node v l (removeRight r))





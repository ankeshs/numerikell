{-|
	Licence Information

	This file is part of Numerikell 1.0.0.0 Haskell Numerical
	Software project. Please do not share, copy, edit or distribute
	without owner's permission.  

	@Contributors : Please striclty follow Haskell community convention.
	Comment your code, use proper nomenclature for functions, variables
	and modules.

	File Specification :
	Contributor : Dipendra K. Misra (dipendrakumarmisra@gmail.com),
-}

module Stats where

import qualified Data.List as List
import qualified Matrix

mean :: (Fractional a) => [a] -> a
mean x = (sum x)/(fromIntegral.length $ x)

kSmall :: (Ord a) => [a] -> Int -> a
kSmall x k = kSmall' (List.sort x) k

kSmall' :: (Ord a) => [a] -> Int -> a
kSmall' x k = x !! k

expV :: (Fractional a) => [a] -> [a] -> a
expV x prob = sum (map  (\(u,v) -> u*v)  ( zip x prob))

centroid :: (Ord a) => [a] -> a
centroid x = kSmall x (quot (length x) 2)

covariance :: (Fractional a) => [a] -> [a] -> a
covariance x y =   (sum (map (\(u,v) -> u*v) $ zip normX normY))/(fromIntegral $ length x - 1)
                where normX =  map (\t -> t-exX) x
                      normY =  map (\t-> t-exY) y
                      exX   =  mean x
                      exY   =  mean y

covMatrix :: (Fractional a) => [[a]] -> [[a]]
covMatrix x = map (\i -> covMatrix' x n i) [0..(n-1)]
            where n = Matrix.maxCol x


covMatrix' :: (Fractional a) => [[a]] -> Int -> Int -> [a]
covMatrix' x n i =  map (\j -> covariance (Matrix.coli x i) (Matrix.coli x j)) [0..(n-1)]


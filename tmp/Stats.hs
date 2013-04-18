-- |This module provides functionalities commonly used in Statistics
module Stats (
  -- * functions
  mean,
  kSmall,
  expV,
  centroid,
  covariance,
  covMatrix,
  pearson
  )where

import qualified Data.List as List
import qualified Matrix


-- |gives the mean of data
mean :: (Fractional a) => [a] -> a
mean x = (sum x)/(fromIntegral.length $ x)

-- |gives the kth smallest element in the data
kSmall :: (Ord a) => [a] -> Int -> a
kSmall x k = kSmall' (List.sort x) k

-- |helping function for kSmall
kSmall' :: (Ord a) => [a] -> Int -> a
kSmall' x k = x !! k

-- |gives the expected value of data
expV :: (Fractional a) => [a] -> [a] -> a
expV x prob = sum (map  (\(u,v) -> u*v)  ( zip x prob))

-- |gives the centroid of data
centroid :: (Ord a) => [a] -> a
centroid x = kSmall x (quot (length x) 2)

-- |gives the covariance of two data sets
covariance :: (Fractional a) => [a] -> [a] -> a
covariance x y =   (sum (map (\(u,v) -> u*v) $ zip normX normY))/(fromIntegral $ length x - 1)
                where normX =  map (\t -> t-exX) x
                      normY =  map (\t-> t-exY) y
                      exX   =  mean x
                      exY   =  mean y

-- |gives the covariance matrix of two data
covMatrix :: (Fractional a) => [[a]] -> [[a]]
covMatrix x = map (\i -> covMatrix' x n i) [0..(n-1)]
            where n = Matrix.maxCol x


-- |helping function for covMatrix
covMatrix' :: (Fractional a) => [[a]] -> Int -> Int -> [a]
covMatrix' x n i =  map (\j -> covariance (Matrix.coli x i) (Matrix.coli x j)) [0..(n-1)]

-- |function to find Pearson product-moment correlation coefficient
pearson :: (Fractional a, Floating a) => [a] -> [a] -> Either String a
pearson x y = if (length x/=length y)
              then Left "Length of vectors must Match"
              else Right (xy/(sqrt(x2)*sqrt(y2)))
            where xmean = mean x
                  ymean = mean y
                  xy    = sum (map (\(u,v)->u*v) (zip x1 y1))
                  x1    = map (\t -> t-xmean) x
                  y1    = map (\t -> t-ymean) y
                  x2    = sum (map (\t -> t*t) x1)
                  y2    = sum (map (\t -> t*t) y1)
                  n     = length x


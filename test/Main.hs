{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}


module Main (main) where

import Naperian
import NaperianExample
import GHC.ForeignPtr
import Data.NetCDF.Store
import Data.Maybe ( fromJust )


u :: Matrix 2 5 Int
u = [ [ 1, 2, 3, 4, 5 ]
    , [ 6, 7, 8, 9, 10 ]
    ]

v4 :: Hyper '[Vector 5, Vector 2] Int
v4 = Prism (Prism (Scalar u))

v5 :: Hyper '[Vector 5, Vector 2, Vector 3] Int
v5 = Prism (Prism (Prism (Scalar a)))

a :: Vector 3 (Vector 2 (Vector 5 Int))
a = fromJust $ fromList $ [b, b1, b2]

b :: Vector 2 (Vector 5 Int)
b = [ [ 1, 2, 3, 4, 5 ]
    , [ 6, 7, 8, 9, 10 ]
    ]

b1 :: Vector 2 (Vector 5 Int)
b1 = [ [ 11, 12, 13, 14, 15 ]
     , [ 16, 17, 18, 19, 20 ]
     ]

b2 :: Vector 2 (Vector 5 Int)
b2 = [ [ 21, 22, 23, 24, 25 ]
     , [ 26, 27, 28, 29, 30 ]
     ]


roundTrip :: Hyper '[Vector 5, Vector 2, Vector 3] Int ->
             Hyper '[Vector 5, Vector 2, Vector 3] Int
roundTrip = unToListLike . toListLike


main :: IO ()
main = do
  print v5
  print $ roundTrip v5
  let scalarPi :: Hyper '[] Double
      scalarPi = Scalar pi
  let fpPi :: ForeignPtr Double
      fpPi = toForeignPtr scalarPi
  let roundTripPi :: Hyper '[] Double
      roundTripPi = fromForeignPtr fpPi undefined
  putStrLn $ show (point scalarPi == point roundTripPi)
  let foo = toForeignPtr $ Prism $ Scalar ([ 1, 2, 3 ] :: Vector 3 Int)
      bar :: Hyper '[Vector 3] Int
      bar = fromForeignPtr foo undefined
  putStrLn $ show bar
  let foo2 = toForeignPtr v4
      bar2 :: Hyper '[Vector 5, Vector 2] Int
      bar2 = fromForeignPtr foo2 undefined
  putStrLn $ show v4
  putStrLn $ show bar2
  let foo3 = toForeignPtr v5
      bar3 :: Hyper '[Vector 5, Vector 2, Vector 3] Int
      bar3 = fromForeignPtr foo3 undefined
  putStrLn $ show v5
  putStrLn $ show bar3

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}


module NaperianExample where

import Data.NetCDF.Store
import Naperian
import GHC.TypeNats
import Data.Proxy

import GHC.ForeignPtr
import Foreign.Storable
import qualified Data.Vector.Storable as SV

import Data.Maybe ( fromJust )

import Data.Foldable       ( toList )

class NcEmpty fs where
  ncEmpty :: fs [e]

instance NcEmpty (Hyper '[]) where
  ncEmpty = Scalar []

instance (KnownNat n, NcEmpty (Hyper fs), Shapely fs) =>
         NcEmpty (Hyper ((Vector n) : fs)) where
  ncEmpty = Prism $ fmap (fromJust . fromList . Prelude.replicate n) ncEmpty
    where
      n = fromIntegral $ natVal (Proxy :: Proxy n)

class Urk fs where
  urk :: fs e -> [e]

instance Urk (Hyper '[]) where
  urk (Scalar c) = [c]

instance (KnownNat n, Urk (Hyper fs)) => Urk (Hyper ((Vector n) : fs)) where
  urk (Prism c) = concat $ urk $ fmap toList c

instance NcStore (Hyper '[]) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr p _ = Scalar . head . SV.toList $ SV.unsafeFromForeignPtr0 p 1

instance (KnownNat n, Shapely fs, NcStore (Hyper fs), NcEmpty (Hyper fs)) =>
         NcStore (Hyper ((Vector n) : fs)) where
  type NcStoreExtraCon (Hyper ((Vector n) : fs)) e = NcStoreExtraCon (Hyper fs) e
  toForeignPtr :: forall e . Storable e =>
                  Hyper ((Vector n) : fs) e -> ForeignPtr e
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . (Storable e, NcStoreExtraCon (Hyper fs) e) =>
                    ForeignPtr e -> [Int] -> Hyper ((Vector n) : fs) e
  fromForeignPtr p _ = Prism zs
    where
      xs :: [Hyper fs e]
      xs = map (\i -> fromForeignPtr (plusForeignPtr p (i * m * l)) undefined)
               [0 .. n - 1]

      ys :: Hyper fs [e]
      ys = foldr (hzipWith (:)) ncEmpty xs

      zs :: Hyper fs (Vector n e)
      zs = fmap (fromJust . fromList) ys

      n = fromIntegral $ natVal (Proxy :: Proxy n)
      -- m = hsize (undefined :: Hyper fs e)
      m = hsize ((xs!!0) :: Hyper fs e)
      l = sizeOf (undefined :: e)

x :: Matrix 2 3 Int
x = [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    ]

v1 :: Hyper '[Vector 3, Vector 2] Int
v1 = Prism (Prism (Scalar x))

y :: Matrix 3 3 Int
y = [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    ]

v2 :: Hyper '[Vector 3, Vector 3] Int
v2 = Prism (Prism (Scalar y))

z :: Matrix 1 1 Int
z = [ [ 1 ] ]

v3 :: Hyper '[Vector 1, Vector 1] Int
v3 = Prism (Prism (Scalar z))

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

main :: IO ()
main = do
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

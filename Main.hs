{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE UndecidableInstances #-}

module NaperianExample where

-- import Data.NetCDF
import Data.NetCDF.Store
-- import Data.NetCDF.Vector
import Naperian
import GHC.TypeNats
import Data.Proxy

import GHC.ForeignPtr
import Foreign.Storable
import qualified Data.Vector.Storable as SV

-- import Data.Foldable ( toList )
import Data.Maybe ( fromJust )

-- import Debug.Trace

instance NcStore (Hyper '[]) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr p _ = Scalar . head . SV.toList $ SV.unsafeFromForeignPtr0 p 1

-- instance (KnownNat n, Shapely fs, NcStore (Hyper fs), NcStoreExtraCon (Hyper fs) e) =>
--          NcStore (Hyper ((Vector n) : fs)) where
--   toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
--   fromForeignPtr :: forall e . (Storable e, NcStoreExtraCon (Hyper fs) e) =>
--                     ForeignPtr e -> [Int] -> Hyper ((Vector n) : fs) e
--   fromForeignPtr p ns = undefined
--     where
--       xs :: [Hyper fs e]
--       xs = map (\n -> fromForeignPtr (plusForeignPtr p (n * m * l)) undefined)
--                [0 .. n - 1]

      -- y0 :: Hyper '[Vector n] [e]
      -- y0 = undefined
      -- y0 = Prism $ Scalar $ fromJust $
      --      fromList $ Prelude.replicate m []

      -- ys :: Hyper '[Vector n] [e]
      -- ys = foldr (hzipWith (:)) y0 xs

      -- n = fromIntegral $ natVal (Proxy :: Proxy n)
      -- m = hsize (undefined :: Hyper fs e)
      -- l = sizeOf (undefined :: e)

instance forall n . (KnownNat n) => NcStore (Hyper ((Vector n) : '[])) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . Storable e =>
                    ForeignPtr e -> [Int] -> Hyper '[Vector n] e
  fromForeignPtr p _ = Prism zs
    where
      xs :: [Hyper '[] e]
      xs = map (\i -> fromForeignPtr (plusForeignPtr p (i * m * l)) undefined)
               [0 .. n - 1]

      ys :: Hyper '[] [e]
      ys = foldr (hzipWith (:)) (Scalar []) xs

      zs :: Hyper '[] (Vector n e)
      zs = fmap (fromJust . fromList) ys

      n = fromIntegral $ natVal (Proxy :: Proxy n)
      m = 1
      l = sizeOf (undefined :: e)

instance (KnownNat n, KnownNat m) => NcStore (Hyper ((Vector n) : '[Vector m])) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . Storable e =>
                    ForeignPtr e -> [Int] -> Hyper '[Vector n, Vector m] e
  fromForeignPtr p _ = transposeH $ Prism zs
    where
      xs :: [Hyper '[Vector n] e]
      xs = map (\i -> fromForeignPtr (plusForeignPtr p (i * n * l)) undefined)
               [0 .. m - 1]

      y0 :: Hyper '[Vector n] [e]
      y0 = Prism $ Scalar $ fromJust $
           fromList $ Prelude.replicate n []

      ys :: Hyper '[Vector n] [e]
      ys = foldr (hzipWith (:)) y0 xs

      zs :: Hyper '[Vector n] (Vector m e)
      zs = fmap (fromJust . fromList) ys

      n = fromIntegral $ natVal (Proxy :: Proxy n)
      m = fromIntegral $ natVal (Proxy :: Proxy m)
      l = sizeOf (undefined :: e)

instance (KnownNat n, KnownNat m, KnownNat q) =>
         NcStore (Hyper ((Vector n) : '[Vector m, Vector q])) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . Storable e =>
                    ForeignPtr e -> [Int] -> Hyper '[Vector n, Vector m, Vector q] e
  fromForeignPtr p _ = r
    where
      xs :: [Hyper '[Vector m, Vector q] e]
      xs = map (\i -> fromForeignPtr (plusForeignPtr p (i * m * q * l)) undefined)
               [0 .. n - 1]

      y0 :: Hyper '[Vector m, Vector q] [e]
      y0 = Prism $ Prism $ Scalar $
           fromJust $ fromList $ Prelude.replicate q $
           fromJust $ fromList $ Prelude.replicate m []

      ys :: Hyper '[Vector m, Vector q] [e]
      ys = foldr (hzipWith (:)) y0 xs

      zs :: Hyper '[Vector m, Vector q] (Vector n e)
      zs = fmap (fromJust . fromList) ys

      r :: Hyper '[Vector n, Vector m, Vector q] e
      r = Prism zs

      n = fromIntegral $ natVal (Proxy :: Proxy n)
      m = fromIntegral $ natVal (Proxy :: Proxy m)
      q = fromIntegral $ natVal (Proxy :: Proxy q)
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
a = fromJust $ fromList $ Prelude.replicate 3 b

b :: Vector 2 (Vector 5 Int)
b = [ [ 1, 2, 3, 4, 5 ]
    , [ 6, 7, 8, 9, 10 ]
    ]

-- foo = transposeH (Prism ((fmap (fromJust . fromList) $ foldr (hzipWith (:)) ((Prism $ Scalar $ fromJust $ fromList $ Prelude.replicate 5 []) :: Hyper '[Vector 5] [Int]) [Prism $ Scalar [1..5] :: Hyper '[Vector 5] Int, Prism $ Scalar [6..10] :: Hyper '[Vector 5] Int]) :: Hyper '[Vector 5] (Vector 2 Int)))

aaa :: Hyper '[Vector 5] [Int]
aaa = Prism $ Scalar $ fromJust $ fromList $ Prelude.replicate 5 []

ccc :: [Hyper '[Vector 5] Int]
ccc = [ Prism $ Scalar [1..5] :: Hyper '[Vector 5] Int
      , Prism $ Scalar [6..10] :: Hyper '[Vector 5] Int]

cccc :: [Hyper '[Vector 5, Vector 2] Int]
cccc = [ Prism $ Prism $ Scalar
         [ [ 1 .. 5 ]
         , [ 6 .. 10 ]
         ]
       , Prism $ Prism $ Scalar
         [ [ 11 .. 15 ]
         , [ 16 .. 20 ]
         ]
       , Prism $ Prism $ Scalar
         [ [ 21 .. 25 ]
         , [ 26 .. 30 ]
         ]
       ]

bbbb :: Hyper fs [Int] -> [Hyper fs Int] -> Hyper fs [Int]
bbbb = foldr (hzipWith (:))

aaaa :: Hyper '[Vector 5, Vector 2] [Int]
aaaa = Prism $ Prism $ Scalar $
       fromJust $ fromList $ Prelude.replicate 2 $
       fromJust $ fromList $ Prelude.replicate 5 []

zzz :: Vector 2 (Vector 5 [Int])
zzz = fromJust $ fromList $ Prelude.replicate 2 $
      fromJust $ fromList $ Prelude.replicate 5 []

dddd :: Hyper '[Vector 5, Vector 2] [Int]
dddd = bbbb aaaa cccc

bbb :: KnownNat n =>
       Hyper '[Vector n] [Int] -> [Hyper '[Vector n] Int] -> Hyper '[Vector n] [Int]
bbb = foldr (hzipWith (:))

ddd :: Hyper '[Vector 5] [Int]
ddd = bbb aaa ccc

eee :: Hyper '[Vector 5] (Vector 2 Int)
eee = fmap (fromJust . fromList) ddd

eeee :: Hyper '[Vector 5, Vector 2] (Vector 3 Int)
eeee = fmap (fromJust . fromList) dddd

fff :: Hyper '[Vector 2, Vector 5] Int
fff = Prism eee

ffff :: Hyper '[Vector 3, Vector 5, Vector 2] Int
ffff = Prism eeee

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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE TypeFamilies #-}

import Data.NetCDF
import Data.NetCDF.Store
import Data.NetCDF.Vector
import Naperian
import GHC.TypeNats
import Data.Proxy

import GHC.ForeignPtr
import Foreign.Storable
import qualified Data.Vector.Storable as SV

import Data.Foldable ( toList )
import Data.Maybe ( fromJust )

import Debug.Trace

instance NcStore (Hyper '[]) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr p _ = Scalar . head . SV.toList $ SV.unsafeFromForeignPtr0 p 1

-- instance (KnownNat n, Shapely fs) => NcStore (Hyper ((Vector n) : fs)) where
--   toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
--   fromForeignPtr p ns = undefined

instance forall n . (KnownNat n) => NcStore (Hyper ((Vector n) : '[])) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . Storable e =>
                    ForeignPtr e -> [Int] -> Hyper '[Vector n] e
  fromForeignPtr p _ = Prism zs
    where
      xs :: [Hyper '[] e]
      xs = map (\n -> fromForeignPtr (plusForeignPtr p (n * m * l)) undefined)
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
  fromForeignPtr p ns = Prism zs
    where
      xs :: [Hyper '[Vector m] e]
      xs = map (\n -> fromForeignPtr (plusForeignPtr p (n * m * l)) undefined)
               [0 .. n - 1]

      y0 :: Hyper '[Vector m] [e]
      y0 = Prism $ Scalar $ fromJust $
           fromList $ Prelude.replicate m []

      ys :: Hyper '[Vector m] [e]
      ys = foldr (hzipWith (:)) y0 xs

      zs :: Hyper '[Vector m] (Vector n e)
      zs = fmap (fromJust . fromList) ys

      n = fromIntegral $ natVal (Proxy :: Proxy n)
      m = fromIntegral $ natVal (Proxy :: Proxy m)
      l = sizeOf (undefined :: e)

x = [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    ] :: Matrix 2 3 Int

v1 = Prism (Prism (Scalar x)) :: Hyper '[Vector 3, Vector 2] Int

y = [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    ] :: Matrix 3 3 Int

v2 = Prism (Prism (Scalar y)) :: Hyper '[Vector 3, Vector 3] Int

z = [ [ 1 ] ] :: Matrix 1 1 Int

v3 = Prism (Prism (Scalar z)) :: Hyper '[Vector 1, Vector 1] Int

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
  let foo2 = toForeignPtr v1
      bar2 :: Hyper '[Vector 3, Vector 2] Int
      bar2 = fromForeignPtr foo2 undefined
  putStrLn $ show v1
  putStrLn $ show bar2

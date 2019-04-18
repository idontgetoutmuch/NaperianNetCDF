{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}


module Data.Naperian.NetCDF () where

import Data.NetCDF.Store
import Naperian
import GHC.TypeNats
import Data.Proxy

import GHC.ForeignPtr
import GHC.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as SV

import Data.Maybe ( fromJust )
import Data.Foldable ( toList )
import Control.Monad

instance (KnownNat n, Storable e) => Storable (Vector n e) where
  poke p v = zipWithM_ poke qs (toList v)
    where
      qs = map (\i -> plusPtr q (l * i)) [0 .. n - 1]
      n = fromIntegral $ natVal (Proxy :: Proxy n)
      l = sizeOf (undefined :: e)
      q = castPtr p
  peek p = do vs <-  mapM peek qs
              return $ fromJust $ fromList vs
    where
      q :: Ptr e
      q = castPtr p
      qs = map (\i -> plusPtr q (l * i)) [0 .. n - 1]
      l = sizeOf (undefined :: e)
      n = fromIntegral $ natVal (Proxy :: Proxy n)
  sizeOf _ = n * sizeOf (undefined :: e)
    where
      n = fromIntegral $ natVal (Proxy :: Proxy n)
  alignment _ = alignment (undefined :: e)

instance NcStore (Hyper '[]) where
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr p _ = Scalar . head . SV.toList $ SV.unsafeFromForeignPtr0 p 1
  smap = error "smap: NcStore (Hyper '[])"

instance (KnownNat n, Shapely fs, NcStore (Hyper fs)) =>
         NcStore (Hyper ((Vector n) : fs)) where
  type NcStoreExtraCon (Hyper ((Vector n) : fs)) e = NcStoreExtraCon (Hyper fs) (Vector n e)
  toForeignPtr :: forall e . Storable e =>
                  Hyper ((Vector n) : fs) e -> ForeignPtr e
  toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
  fromForeignPtr :: forall e . (Storable e , NcStoreExtraCon (Hyper fs) (Vector n e)) =>
                    ForeignPtr e -> [Int] -> Hyper ((Vector n) : fs) e
  fromForeignPtr p _ = Prism ws
    where
      q :: ForeignPtr (Vector n e)
      q = castForeignPtr p

      ws :: Hyper fs (Vector n e)
      ws = fromForeignPtr q undefined
  smap = error "smap: NcStore (Hyper ((Vector n) : fs))"


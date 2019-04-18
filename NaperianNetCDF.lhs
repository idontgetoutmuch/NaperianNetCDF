% Data Sources
% Dominic Steinitz
% 17th February 2019

Introduction
============

Suppose you have some array-like data store in
[NetCDF](https://en.wikipedia.org/wiki/NetCDF) such as [surface sea
temperature](https://www.unidata.ucar.edu/software/netcdf/examples/files.html)
at given positiosn (latitude and longitude) over time. It's possible
to do
[APL](https://en.wikipedia.org/wiki/APL_(programming_language))-like
programming in Haskell using [Naperian
functors](https://www.cs.ox.ac.uk/publications/publication10857-abstract.html). APL-like
programming seems like a good fit for analysing such data but how does
one get such data from a NetCDF file into an array in which each
dimension is typed with the size of the data in that dimension?

[`Data.NetCDF.Store`](https://hackage.haskell.org/package/hnetcdf-0.5.0.0/docs/Data-NetCDF-Store.html)
contains the `NcStore` store class. So all we need to is implement this class for the appropriate structure in the [Naperian](http://hackage.haskell.org/package/Naperian-0.1.1.0/docs/Naperian.html) package.

Before we start having fun with pointers, let's try implementing a
simpler but similar class.

First we need some extensions and to import the necessary modules.

> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE FlexibleInstances   #-}
> {-# LANGUAGE TypeFamilies        #-}
> {-# LANGUAGE ExplicitForAll      #-}
> {-# LANGUAGE TypeOperators       #-}
> {-# LANGUAGE FlexibleContexts    #-}
> {-# LANGUAGE InstanceSigs        #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedLists     #-}
> {-# LANGUAGE Rank2Types          #-}

> module NaperianNetCDF where

> import Naperian
> import Data.Proxy
> import GHC.TypeLits
> import Data.List.Split
> import Data.Maybe
> import Data.Foldable
> import Data.NetCDF.Store
> import Data.NetCDF
> import qualified Data.Vector.Storable as SV
> import GHC.ForeignPtr
> import Foreign.Storable
> import Foreign.C
> import GHC.Ptr
> import Control.Monad

Here's the class definition.

> class ListLike fs where
>   toListLike :: fs e -> [e]
>   unToListLike :: [e] -> fs e

The base instance for the hypercuboid of the empty list of types is
straightforward.

> instance ListLike (Hyper '[]) where
>   toListLike (Scalar c) = [c]
>   unToListLike [c] = Scalar c
>   unToListLike cs  = error $ "ToListLike: wrong length = " ++ show (Prelude.length cs)

The instance for recursion also seems simple in retrospect but I took
many wrong turns before getting here.

> instance (KnownNat n, ListLike (Hyper fs), Shapely fs) =>
>          ListLike (Hyper ((Vector n) : fs)) where
>   toListLike (Prism c) = concat $ toListLike $ fmap toList c
>   unToListLike :: forall e . [e] -> Hyper ((Vector n) : fs) e
>   unToListLike cs = Prism $ unToListLike us
>     where
>       us :: [Vector n e]
>       us = map (fromJust . fromList) $
>            chunksOf n cs
>
>       n = fromIntegral $ natVal (Proxy :: Proxy n)

Let's check things are working as intended.

> roundTrip :: Hyper '[Vector 5, Vector 2, Vector 3] Int ->
>              Hyper '[Vector 5, Vector 2, Vector 3] Int
> roundTrip = unToListLike . toListLike

> v5 :: Hyper '[Vector 5, Vector 2, Vector 3] Int
> v5 = Prism (Prism (Prism (Scalar a)))

> a :: Vector 3 (Vector 2 (Vector 5 Int))
> a = fromJust $ fromList $ [b, b1, b2]

> b :: Vector 2 (Vector 5 Int)
> b = [ [ 1, 2, 3, 4, 5 ]
>     , [ 6, 7, 8, 9, 10 ]
>     ]

> b1 :: Vector 2 (Vector 5 Int)
> b1 = [ [ 11, 12, 13, 14, 15 ]
>      , [ 16, 17, 18, 19, 20 ]
>      ]

> b2 :: Vector 2 (Vector 5 Int)
> b2 = [ [ 21, 22, 23, 24, 25 ]
>      , [ 26, 27, 28, 29, 30 ]
>      ]


    *NaperianNetCDF> roundTrip v5
    <<<1,2,3,4,5>,<6,7,8,9,10>>,
     <<11,12,13,14,15>,<16,17,18,19,20>>,
     <<21,22,23,24,25>,<26,27,28,29,30>>>

    *NaperianNetCDF> v5
    <<<1,2,3,4,5>,<6,7,8,9,10>>,
     <<11,12,13,14,15>,<16,17,18,19,20>>,
     <<21,22,23,24,25>,<26,27,28,29,30>>>

Now let's implement an instance for the type class that we really want.

Again the base instance is straightforward.

> instance NcStore (Hyper '[]) where
>   type instance NcStoreExtraCon (Hyper '[]) a = ()
>   toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
>   fromForeignPtr p _ = Scalar . head . SV.toList $ SV.unsafeFromForeignPtr0 p 1
>   smap = error "smap: NcStore (Hyper '[])"

> instance (KnownNat n, Shapely fs, NcStore (Hyper fs)) =>
>          NcStore (Hyper ((Vector n) : fs)) where
>   type NcStoreExtraCon (Hyper ((Vector n) : fs)) e = NcStoreExtraCon (Hyper fs) (Vector n e)
>   toForeignPtr :: forall e . Storable e =>
>                   Hyper ((Vector n) : fs) e -> ForeignPtr e
>   toForeignPtr = fst . SV.unsafeToForeignPtr0 . SV.fromList . elements
>   fromForeignPtr :: forall e . (Storable e , NcStoreExtraCon (Hyper fs) (Vector n e)) =>
>                     ForeignPtr e -> [Int] -> Hyper ((Vector n) : fs) e
>   fromForeignPtr p _ = Prism ws
>     where
>       q :: ForeignPtr (Vector n e)
>       q = castForeignPtr p

>       ws :: Hyper fs (Vector n e)
>       ws = fromForeignPtr q undefined
>   smap = error "smap: NcStore (Hyper ((Vector n) : fs))"

This won't compile complaining that there is no instance for
`Storable` for `Vector n a`. So let's create such an instance.

> instance (KnownNat n, Storable e) => Storable (Vector n e) where
>   poke p v = zipWithM_ poke qs (toList v)
>     where
>       qs = map (\i -> plusPtr q (l * i)) [0 .. n - 1]
>       n = fromIntegral $ natVal (Proxy :: Proxy n)
>       l = sizeOf (undefined :: e)
>       q = castPtr p
>   peek p = do vs <-  mapM peek qs
>               return $ fromJust $ fromList vs
>     where
>       q :: Ptr e
>       q = castPtr p
>       qs = map (\i -> plusPtr q (l * i)) [0 .. n - 1]
>       l = sizeOf (undefined :: e)
>       n = fromIntegral $ natVal (Proxy :: Proxy n)
>   sizeOf _ = n * sizeOf (undefined :: e)

>     where
>       n = fromIntegral $ natVal (Proxy :: Proxy n)
>   alignment _ = alignment (undefined :: e)

One thing I have glossed over is the fact that the `NcStore` typeclass
has a constraint. We can't ignore this in our instance. Effectively we
are saying that the constraint `NcStoreExtraCon (Hyper ((Vector n) :
fs)) e` is satisfied if `NcStoreExtraCon (Hyper fs) (Vector n e)` is
satisifed. This constraint also has to be in the defintion of
`fromForeignPtr` so that the constraint on the call of
`fromForeignPtr` at the smaller type is satisfied. Ultimately these
constraints all evaluate (at compile time) to `()`.

Now we can do some analysis of our data knowing that at compile time
it has consistent dimensions.

> f :: (KnownNat m, KnownNat n) =>
>      Hyper '[Vector m, Vector n] Double ->
>      Hyper '[Vector m, Vector n] Double ->
>      IO ()
> f x y = do
>   print "Maximum difference in latitude bounds"
>   print $ foldrH max (read "-Infinity" :: Double) $
>           fmap abs $
>           foldrH (-) 0 $ transposeH x
>   print "Maximum difference in longitude bounds"
>   print $ foldrH max (read "-Infinity" :: Double) $
>           fmap abs $
>           foldrH (-) 0 $ transposeH y
>   return ()

Of course we need a way of tying together the dynamic nature of
external data with our statically consistent program.

> type NaperianRet2 m n a = IO (Either NcError (Hyper '[Vector m, Vector n] a))

> withNc :: NcInfo NcRead ->
>          (forall m n . (KnownNat m, KnownNat n) =>
>                        Hyper '[Vector m, Vector n] Double ->
>                        Hyper '[Vector m, Vector n] Double ->
>                        IO ()) ->
>          IO ()
> withNc nc f = do
>   case ncVar nc "lat_bnds" of
>     Nothing -> error "Missing lat_bnds"
>     Just latBnds -> case ncVar nc "lon_bnds" of
>       Nothing -> error "Missing lon_bnds"
>       Just lonBnds -> do
>         let dims = ncVarDims latBnds
>         let ls = map ncDimLength dims
>         case someNatVal (fromIntegral (ls!!0)) of
>           Nothing -> error "static / dynamic mismatch"
>           Just (SomeNat (_ :: Proxy aa)) ->
>             case someNatVal (fromIntegral (ls!!1)) of
>               Nothing -> error "static / dynamic mismatch"
>               Just (SomeNat (_bb :: Proxy bb)) -> do
>                 elat <- get nc latBnds :: NaperianRet2 aa bb CDouble
>                 case elat of
>                   Left ncErr1 -> error $ show ncErr1
>                   Right lat -> do
>                     elon <- get nc lonBnds :: NaperianRet2 aa bb CDouble
>                     case elon of
>                       Left ncErr2 -> error $ show ncErr2
>                       Right lon -> f (fmap realToFrac lat) (fmap realToFrac lon)

> main :: IO ()
> main = do
>   enc <- openFile "/Users/dom/Downloads/tos_O1_2001-2002.nc"
>   case enc of
>     Left e -> error $ show e
>     Right nc -> withNc nc f

And finally get our answer (for what it's worth):

    "Maximum difference in latitude bounds"
    85.0
    "Maximum difference in longitude bounds"
    170.0


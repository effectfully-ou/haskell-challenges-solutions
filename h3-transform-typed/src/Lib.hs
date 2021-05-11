-- See also András Kovács' solution with 'Coercible' (the one that is commented out):
-- https://gist.github.com/AndrasKovacs/de59d17dcc637edefb0be0055822654c

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import           Data.Proxy
import           Data.Typeable

import           Data.Coerce

data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a

newtype Wrap a = Wrap
    { unWrap :: a
    }

wrapFunction :: Function -> Function
wrapFunction (Function sch x) = go sch $ \sch' toX' -> Function sch' $ toX' x where
    go :: Scheme a -> (forall a'. Scheme a' -> (a -> a') -> Function) -> Function
    go (Res _)     k = k (Res Proxy) Wrap
    go (Arg _ sch) k = go sch $ \sch' toX' -> k (Arg Proxy sch') $ \f -> toX' . f . unWrap

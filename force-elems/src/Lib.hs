module Lib
    ( forceElems
    ) where

import           Control.Applicative

data Force a = Force
    { runForce :: a
    }

instance Functor Force where
    fmap = liftA

instance Applicative Force where
    pure = Force
    Force f <*> ~a@(Force x) = Force $ a `seq` f x

forceElems :: Traversable t => t a -> t a
forceElems = runForce . traverse (pure $!)

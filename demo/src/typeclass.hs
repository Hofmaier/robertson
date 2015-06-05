{-# LANGUAGE FlexibleInstances #-} 
import Data.Monoid
import Control.Applicative 

instance (Applicative f, Monoid a) => Monoid (f a) where
    mempty = pure mempty
    mappend = liftA2 mappend


--instance Monoid b => Monoid (IO b) where
--  mempty = pure mempty
--  mappend = liftA2 mappend

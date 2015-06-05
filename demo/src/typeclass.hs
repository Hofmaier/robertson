{-# LANGUAGE FlexibleInstances #-} 
import Data.Monoid
import Control.Applicative 

showlist :: Show t => [t] -> String
showlist [] = ""
showlist (x:xs) = show x ++ showlist xs

instance (Applicative f, Monoid a) => Monoid (f a) where
    mempty = pure mempty
    mappend = liftA2 mappend


--instance Monoid b => Monoid (IO b) where
--  mempty = pure mempty
--  mappend = liftA2 mappend

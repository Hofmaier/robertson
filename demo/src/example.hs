{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
import Data.Monoid 
import Control.Applicative 
import System.IO
import Control.Monad

instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty = pure mempty
  mappend = liftA2 mappend

main = do
    c <- getChar
    logto c

print2stdout :: Char -> IO ()
print2stdout c = putChar c

logto :: Char -> IO ()
logto c  = do
  handle <- openFile "log.txt" WriteMode
  hPutChar handle c

composedPlugin :: Char -> IO ()
composedPlugin  = logto `mappend` print2stdout

composed1 = logto `mappend` (print2stdout `mappend` donothing)
composed2 = (logto `mappend` print2stdout) `mappend` donothing

composed3 = logto `mappend` print2stdout 
composed4 = print2stdout `mappend` logto 

donothing :: Char -> IO ()
donothing = \c -> return ()

logto' :: IO ( Char -> IO ())
logto' = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)
  
print2stdout' :: IO ( Char -> IO ())
print2stdout' = return (putChar)

--instance Monoid a => Monoid (IO a) where
--  mempty = pure mempty
--  mappend = liftA2 mappend
 
--composePlugin :: Char -> IO ()
--composePlugin = mappend logto print2stdout'

--instance Monoid b => Monoid (IO b) where
--  mempty = pure mempty
--  mappend = liftA2 mappend

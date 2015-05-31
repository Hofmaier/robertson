module Main where

import Pipes
import qualified Pipes.Prelude as P
import System.IO
import Data.Monoid

main = do
    handleChar <- logto <> print2stdout
    c <- getChar
    handleChar c

takechar :: Char -> IO ()
takechar c = return ()

--donothing :: IO ( Char -> IO ())
--donothing = return 

logto :: IO ( Char -> IO ())
logto = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)

print2stdout :: IO ( Char -> IO ())
print2stdout = do
  handle <- openFile "log1.txt" WriteMode
  return (hPutChar handle)

--instance Monoid b => Monoid (a -> b) where
--    mempty = \_ -> mempty
--    mappend f g = \a -> mappend (f a) (g a)

instance Monoid a => Monoid (IO a) where
    mempty = return mempty

    mappend io1 io2 = do
        a1 <- io1
        a2 <- io2
        return (mappend a1 a2)

producer1 :: Producer String IO ()
producer1 = P.stdinLn

produceints123 :: Producer Int IO ()
produceints123 = each [1,2,3]

print2stdout:: Int -> Effect IO ()
print2stdout x = (lift . putStrLn . show ) x

loop2 :: Effect IO ()
loop2 = for produceints123 print2stdout

duplicate :: Int -> Producer Int IO ()
duplicate x = yield x >> yield x 

composed_producer :: Producer Int IO ()
composed_producer = for produceints123 duplicate

effect2 :: Effect IO ()
effect2 = for composed_producer print2stdout

composed_with_yield :: Int -> Effect IO ()
composed_with_yield = duplicate ~> print2stdout

multiply :: Int -> Producer Int IO ()
multiply x = yield (x * 3)

compose_123_multiply :: Producer Int IO ()
compose_123_multiply = for produceints123 multiply

withyield1 = multiply ~> addproducer ~> print2stdout
withyield2 = addproducer ~> multiply ~> print2stdout

loop3 :: Effect IO ()
loop3 = for compose_123_multiply print2stdout

loop4 = for produceints123 withyield1
loop5 = for produceints123 withyield2

addproducer :: Int -> Producer Int IO ()
addproducer x = yield (x + 3)

compose3 :: Producer Int IO ()
compose3 = for compose_123_multiply addproducer

compose4 :: Producer String IO ()
compose4 = for compose3 int2str

compose5 :: Effect IO ()
compose5 = for compose3 print2stdout


loop :: Effect IO ()
loop = for produceints123 (\i -> do 
                 lift (putStrLn (show i)))

int2str ::Int -> Producer String IO ()
int2str x = yield (show x) 

applicativev :: Maybe (Int -> Int -> Int)
applicativev = Just (+)



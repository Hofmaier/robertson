module Main where

import Pipes
import qualified Pipes.Prelude as P

main = do
    handleChar <- logto
    c <- getChar
    handleChar c

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



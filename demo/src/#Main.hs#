module Main where

import Pipes
import qualified Pipes.Prelude as P

main = putStrLn "pipesdemo output"

r :: Producer String IO ()
r = P.stdinLn

l :: Producer Int IO ()
l = each [1,2,3]

loop :: Effect IO ()
loop = for l (\i -> do 
                 lift (putStrLn (show i)))

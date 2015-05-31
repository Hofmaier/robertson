import System.IO
import Data.Monoid

logto :: IO ( Char -> IO ())
logto = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)

print2stdout :: IO ( Char -> IO ())
print2stdout = return (putChar)

composedPlugin :: IO ( Char -> IO ())
composedPlugin = mappend logto print2stdout

instance Monoid a => Monoid (IO a) where
    mempty = return mempty
    mappend io1 io2 = do
        a1 <- io1
        a2 <- io2
        return (mappend a1 a2)

composed1 = mappend logto print2stdout
composed2 = mappend print2stdout logto

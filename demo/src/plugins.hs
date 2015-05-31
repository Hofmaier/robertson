import System.IO
import Data.Monoid

takechar :: Char -> IO ()
takechar = \c -> return ()

donothing :: IO ( Char -> IO ())
donothing = return (\c -> return ())

main = getChar >>= (\c -> putChar c)


logto :: IO ( Char -> IO ())
logto = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)


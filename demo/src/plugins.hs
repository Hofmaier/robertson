import System.IO
import Data.Monoid

takechar :: Char -> IO ()
takechar = \c -> return ()

main = getChar >>= (\c -> putChar c)


logto :: IO ( Char -> IO ())
logto = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)


import System.IO
logto :: IO ( Char -> IO ())
logto = do
  handle <- openFile "log.txt" WriteMode
  return (hPutChar handle)

module Paths_pipesdemo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lukas/robertson/demo/.cabal-sandbox/bin"
libdir     = "/home/lukas/robertson/demo/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/pipesdemo-0.1.0.0"
datadir    = "/home/lukas/robertson/demo/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/pipesdemo-0.1.0.0"
libexecdir = "/home/lukas/robertson/demo/.cabal-sandbox/libexec"
sysconfdir = "/home/lukas/robertson/demo/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pipesdemo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pipesdemo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "pipesdemo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pipesdemo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pipesdemo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

module Paths_monadcryptorandom (
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
version = Version [0,7,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/eric/.cabal/bin"
libdir     = "/home/eric/.cabal/lib/x86_64-linux-ghc-7.10.2/monad_7ZeaGWy7YKoFksTSkuaW5J"
datadir    = "/home/eric/.cabal/share/x86_64-linux-ghc-7.10.2/monadcryptorandom-0.7.1"
libexecdir = "/home/eric/.cabal/libexec"
sysconfdir = "/home/eric/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monadcryptorandom_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monadcryptorandom_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "monadcryptorandom_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monadcryptorandom_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monadcryptorandom_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

module Paths_miller_rabin (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mukeshtiwari/.cabal/bin"
libdir     = "/Users/mukeshtiwari/.cabal/lib/miller-rabin-0.1.0.0/ghc-7.6.1"
datadir    = "/Users/mukeshtiwari/.cabal/share/miller-rabin-0.1.0.0"
libexecdir = "/Users/mukeshtiwari/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "miller_rabin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "miller_rabin_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "miller_rabin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "miller_rabin_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

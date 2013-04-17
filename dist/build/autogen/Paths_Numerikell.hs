module Paths_Numerikell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ankeshs/.cabal/bin"
libdir     = "/home/ankeshs/.cabal/lib/Numerikell-1.0/ghc-7.4.2"
datadir    = "/home/ankeshs/.cabal/share/Numerikell-1.0"
libexecdir = "/home/ankeshs/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Numerikell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Numerikell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Numerikell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Numerikell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

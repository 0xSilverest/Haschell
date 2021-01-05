{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SchemeInterpreter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/silverest/.cabal/bin"
libdir     = "/home/silverest/.cabal/lib/x86_64-linux-ghc-8.10.2/SchemeInterpreter-0.1.0.0-inplace-SchemeInterpreter-exe"
dynlibdir  = "/home/silverest/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/silverest/.cabal/share/x86_64-linux-ghc-8.10.2/SchemeInterpreter-0.1.0.0"
libexecdir = "/home/silverest/.cabal/libexec/x86_64-linux-ghc-8.10.2/SchemeInterpreter-0.1.0.0"
sysconfdir = "/home/silverest/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SchemeInterpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SchemeInterpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SchemeInterpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SchemeInterpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SchemeInterpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SchemeInterpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

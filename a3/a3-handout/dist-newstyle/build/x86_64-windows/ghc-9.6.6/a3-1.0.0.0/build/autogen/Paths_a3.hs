{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_a3 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\bin"
libdir     = "C:\\cabal\\x86_64-windows-ghc-9.6.6\\a3-1.0.0.0-inplace"
dynlibdir  = "C:\\cabal\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\cabal\\x86_64-windows-ghc-9.6.6\\a3-1.0.0.0"
libexecdir = "C:\\cabal\\a3-1.0.0.0-inplace\\x86_64-windows-ghc-9.6.6\\a3-1.0.0.0"
sysconfdir = "C:\\cabal\\etc"

getBinDir     = catchIO (getEnv "a3_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "a3_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "a3_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "a3_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "a3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "a3_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'

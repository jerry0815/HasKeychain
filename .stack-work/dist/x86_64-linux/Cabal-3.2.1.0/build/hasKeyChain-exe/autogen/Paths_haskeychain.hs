{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskeychain (
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

bindir     = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/bin"
libdir     = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/lib/x86_64-linux-ghc-8.10.7/haskeychain-0.1.0.0-2fjkBYNVfhw9N0gClthSr2-haskeychain-exe"
dynlibdir  = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/share/x86_64-linux-ghc-8.10.7/haskeychain-0.1.0.0"
libexecdir = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/libexec/x86_64-linux-ghc-8.10.7/haskeychain-0.1.0.0"
sysconfdir = "/mnt/c/jerry/ucsd/230/HasKeychain/.stack-work/install/x86_64-linux/a2dae9b590e16ea5a04fca9d628e4c256bebb6ee36558b9993e06590851dfcf6/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskeychain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskeychain_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskeychain_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskeychain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskeychain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskeychain_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

module Paths_groebner (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\tak\\repo\\cox_grobner\\code\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\tak\\repo\\cox_grobner\\code\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\groebner-0.1.0.0"
datadir    = "C:\\Users\\tak\\repo\\cox_grobner\\code\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\groebner-0.1.0.0"
libexecdir = "C:\\Users\\tak\\repo\\cox_grobner\\code\\.cabal-sandbox\\groebner-0.1.0.0"
sysconfdir = "C:\\Users\\tak\\repo\\cox_grobner\\code\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "groebner_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "groebner_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "groebner_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "groebner_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "groebner_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

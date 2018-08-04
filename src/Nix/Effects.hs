module Nix.Effects where

import Data.Text (Text)
import Nix.Render
import Nix.Value
import System.Posix.Files

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class MonadFile m => MonadEffects m where
    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

    toFile_ :: FilePath -> String -> m StorePath

    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath
    findEnvPath :: String -> m FilePath

    -- | Having an explicit list of sets corresponding to the NIX_PATH
    -- and a file path try to find an existing path
    findPath :: [NThunk m] -> FilePath -> m FilePath

    pathExists :: FilePath -> m Bool
    importPath :: FilePath -> m (NValue m)
    pathToDefaultNix :: FilePath -> m FilePath

    getEnvVar :: String -> m (Maybe String)
    getCurrentSystemOS :: m Text
    getCurrentSystemArch :: m Text

    listDirectory :: FilePath -> m [FilePath]
    getSymbolicLinkStatus :: FilePath -> m FileStatus

    derivationStrict :: NValue m -> m (NValue m)

    nixInstantiateExpr :: String -> m (NValue m)

    getURL :: Text -> m (NValue m)

    getRecursiveSize :: a -> m (NValue m)

    traceEffect :: String -> m ()

    exec :: [String] -> m (NValue m)


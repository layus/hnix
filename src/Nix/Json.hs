{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Json where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Free
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encoding           as A
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import qualified Data.Vector                   as V
import           Nix.Atoms
import           Nix.Effects
import           Nix.Exec
import           Nix.Frames
import           Nix.String
import           Nix.Utils
import           Nix.Value
import           Nix.Value.Monad

nvalueToJSONNixString :: MonadNix e t f m => Bool -> NValue t f m -> m NixString
nvalueToJSONNixString strict =
  runWithStringContextT .
    fmap
      ( TL.toStrict
      . TL.decodeUtf8
      . A.encodingToLazyByteString
      . toEncodingSorted
      )

      . nvalueToJSON strict

nvalueToJSON :: MonadNix e t f m => Bool -> NValue t f m -> WithStringContextT m A.Value
nvalueToJSON strict = iterNValueM toJsonError handleThunk phi
 where
  handleThunk :: MonadNix e t f m => t -> (NValue t f m -> WithStringContextT m A.Value) -> WithStringContextT m A.Value
  handleThunk t _ | not strict = cannotCoerce (Pure t)
  handleThunk t f | otherwise = join $ fmap f $ lift $ demand (Pure t)

  cannotCoerce :: MonadNix e t f m => NValue t f m -> WithStringContextT m A.Value
  cannotCoerce v = lift $ throwError $ CoercionToJson v

  -- We do not expect to reach this machinery
  -- It means that you have tried to evaluate a function provided inside a CoercionToJson error frame.
  toJsonError = error "This is a stub that you should never hit"

  phi :: MonadNix e t f m => NValue' t f m (WithStringContextT m A.Value) -> WithStringContextT m A.Value
  phi = \case
    NVConstant' (NInt   n) -> pure $ A.toJSON n
    NVConstant' (NFloat n) -> pure $ A.toJSON n
    NVConstant' (NBool  b) -> pure $ A.toJSON b
    NVConstant' (NURI   s) -> pure $ A.toJSON s
    NVConstant' NNull      -> pure   A.Null
    NVStr' ns -> A.toJSON <$> extractNixString ns
    NVList' l -> A.Array . V.fromList <$> sequence l
    NVSet' m _ ->
      maybe
        (A.Object <$> sequence m)
        id
        (HM.lookup "outPath" m)
    NVPath' p ->
      do
        fp <- lift $ unStorePath <$> addPath p
        addSingletonStringContext $ StringContext (Text.pack fp) DirectPath
        pure $ A.toJSON fp
    -- Because we are inside iterNValueM, the nv we receive here is not an
    -- (NValue t f m), it is an (NValue' t f m (n r)) and hence cannot be used
    -- inside CoercionToJson error frame.
    -- Luckily, the values that we cannot dump as json are functions, and thus
    -- cannot be used in the error message, so we simply discard them here.
    -- We keep both identical cases separate so as to ensure that we do not
    -- fall here for cases we should otherwise handle.
    -- Building the error frame this way preserves annotations that may be
    -- useful in the error message
    nv@(NVClosure' _ _) -> cannotCoerce $ Free (fmap (const toJsonError) nv)
    nv@(NVBuiltin' _ _) -> cannotCoerce $ Free (fmap (const toJsonError) nv)

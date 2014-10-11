{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath (CanonicalPath
                                ,canonicalPath
                                ,canonicalPathM
                                ,canonicalPathE) where

import           BasicPrelude
import qualified Data.String as S
import           Filesystem.CanonicalPath.Internal
import           Prelude (Show)

data CanonicalPath = CanonicalPath UnsafePath

instance Show CanonicalPath where
  showsPrec d path =
    showParen (d > 15)
              (showString "CanonicalPath " .
               shows (toText path))
    where toText (CanonicalPath path) = pathToText path

canonicalPath :: UnsafePath -> IO CanonicalPath
canonicalPath path = canonicalize path >>= either (error . textToString) (return . CanonicalPath)

canonicalPathM :: UnsafePath -> IO (Maybe CanonicalPath)
canonicalPathM path = canonicalize path >>= either (\_ -> return Nothing) (return . Just . CanonicalPath)

canonicalPathE :: UnsafePath -> IO (Either Text CanonicalPath)
canonicalPathE path = canonicalize path >>= either (return . Left) (return . Right . CanonicalPath)

{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Internal (UnsafePath
                                                     ,SafePath
                                                     ,canonicalize
                                                     ,extractPath
                                                     ,pathToText) where

import           BasicPrelude
import           Control.Applicative as Applicative
import           Control.Arrow (left)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Prelude
import           System.Directory (getHomeDirectory
                                  ,canonicalizePath
                                  ,doesDirectoryExist
                                  ,doesFileExist)
import qualified System.Environment as SE (getEnv)

type UnsafePath = FilePath.FilePath
type SafePath = Either Text UnsafePath

instance Monoid b => Monoid (Either a b) where
  mempty = Right mempty
  mappend l@(Left _) _ = l
  mappend _ l@(Left _) = l
  mappend (Right a)(Right b) = Right $ mappend a b

canonicalize :: UnsafePath -> IO SafePath
canonicalize fp =
  do exists <- liftM2 (||) (doesFileExist . toPrelude $ fp) (doesDirectoryExist . toPrelude $ fp)
     if exists
        then liftM Right (pathMap canonicalizePath fp)
        else return . Left $ "Path does not exist (no such file or directory): " ++ pathToText fp

extractPath :: UnsafePath -> IO SafePath
extractPath = concatM . map extractAtom . FilePath.splitDirectories

extractAtom :: UnsafePath -> IO SafePath
extractAtom atom = tryEnvPosix <||> tryEnvWindows <||> tryHome <%> atom

-- Parsers and parser combinators

type Parser = UnsafePath -> Maybe (IO SafePath)

tryEnvPosix :: Parser
tryEnvPosix x = when' (hasPrefix "$" x) (Just . getEnv . pathTail $ x)

-- TODO: define
tryEnvWindows :: Parser
tryEnvWindows _ = Nothing

tryHome :: Parser
tryHome x = when' (textToPath "~" == x) (Just $ liftM Right homeDirectory)

(<||>) :: Parser -> Parser -> Parser
p1 <||> p2 = \v -> case p1 v of
                     Nothing -> p2 v
                     Just v' -> Just v'

(<%>) :: Parser -> UnsafePath -> IO SafePath
p <%> v = fromMaybe (return . Right $ v) (p v)

-- Utilities

getEnv :: UnsafePath -> IO SafePath
getEnv var = map (left show) tryEnv
  where env = pathMap SE.getEnv
        tryEnv :: IO (Either IOException UnsafePath)
        tryEnv = try . env $ var

homeDirectory :: IO UnsafePath
homeDirectory = map fromPrelude getHomeDirectory

concatM :: (Monad m, Applicative m, Monoid a) => [m a] -> m a
concatM =
  foldl' (\a b -> mappend <$> a <*> b)
         (return mempty)

when' :: Alternative f => Bool -> f a -> f a
when' b v = if b then v else Applicative.empty

pathMap :: (Prelude.FilePath -> IO Prelude.FilePath) -> UnsafePath -> IO UnsafePath
pathMap f = fmap fromPrelude . f . toPrelude

hasPrefix :: Text -> UnsafePath -> Bool
hasPrefix prefix path = prefix `Text.isPrefixOf` pathToText path

hasSuffix :: Text -> UnsafePath -> Bool
hasSuffix suffix path = suffix `Text.isSuffixOf` pathToText path

pathTail :: UnsafePath -> UnsafePath
pathTail = textToPath . Text.tail . pathToText

-- Type conversions

pathToText :: UnsafePath -> Text
pathToText s =
  case FilePath.toText s of
    Left e -> error . textToString $ e
    Right t -> t

textToPath :: Text -> UnsafePath
textToPath = FilePath.fromText

fromPrelude :: Prelude.FilePath -> UnsafePath
fromPrelude = textToPath . Text.pack

toPrelude :: UnsafePath -> Prelude.FilePath
toPrelude = Text.unpack . pathToText

{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Internal (CanonicalPath(..)
                                         ,canonicalPath
                                         ,canonicalPath'
                                         ,canonicalPathM
                                         ,canonicalPathM'
                                         ,canonicalPathE
                                         ,canonicalPathE'
                                         ,unsafePath
                                         ,Filesystem.CanonicalPath.Internal.readFile
                                         ,Filesystem.CanonicalPath.Internal.writeFile
                                         ,writeFile'
                                         ,Filesystem.CanonicalPath.Internal.appendFile
                                         ,preludeMap
                                         ,fromText
                                         ,toText
                                         ,toText'
                                         ,toPrelude
                                         ,fromPrelude
                                         ,voidM) where

import           BasicPrelude
import           Control.Applicative as Applicative
import           Control.Arrow (left, right)
import           Data.Text ()
import qualified Data.Text as Text
import           Filesystem.Path.CurrentOS
import qualified Prelude
import           System.Directory (getHomeDirectory
                                  ,canonicalizePath)
import qualified System.Environment as SE (getEnv)

newtype CanonicalPath = CanonicalPath FilePath deriving Eq

instance Show CanonicalPath where
  showsPrec d path =
    showParen (d > 15)
              (showString "CanonicalPath " .
               shows (toText' path))

{-|
Unsafe constructor of @CanonicalPath@. In case of any problem it will @error@.

Example:

>>> canonicalPath "$HOME"
CanonicalPath "/Users/your-user-name"

>>> canonicalPath "unknown"
*** Exception: Path does not exist (no such file or directory): unknown

/Since 0.1.0.0/
-}
canonicalPath :: MonadIO m => FilePath -> m CanonicalPath
canonicalPath path = canonicalize path >>= either (error . textToString) (return . CanonicalPath)

{-|
Version of @canonicalPath@ that takes @Text@ instead of @FilePath@.

/Since 0.2.1.0/
-}
canonicalPath' :: MonadIO m => Text -> m CanonicalPath
canonicalPath' = canonicalPath . fromText

{-|
Constructs @Maybe CanonicalPath@.

>>> canonicalPathM "~"
Just CanonicalPath "Users/your-user-name"

>>> canonicalPathM "unknown"
Nothing

/Since 0.1.0.0/
-}
canonicalPathM :: MonadIO m => FilePath -> m (Maybe CanonicalPath)
canonicalPathM path = canonicalize path >>= either (\_ -> return Nothing) (return . Just . CanonicalPath)

{-|
Version of @canonicalPathM@ that takes @Text@ instead of @FilePath@.

/Since 0.2.1.0/
-}
canonicalPathM' :: MonadIO m => Text -> m (Maybe CanonicalPath)
canonicalPathM' = canonicalPathM . fromText

{-|
Constructs @Either Text CanonicalPath@.

>>> canonicalPathE "~/"
Right CanonicalPath "/Users/your-user-name"

>>> canonicalPathE "$HOME/this-folder-does-not-exist"
Left "Path does not exist (no such file or directory): /Users/your-user-name/this-folder-does-not-exist"

/Since 0.1.0.0/
-}
canonicalPathE :: MonadIO m => FilePath -> m (Either Text CanonicalPath)
canonicalPathE path = canonicalize path >>= either (return . Left) (return . Right . CanonicalPath)

{-|
Version of @canonicalPathE@ that takes @Text@ instead of @FilePath@.

/Since 0.2.1.0/
-}
canonicalPathE' :: MonadIO m => Text -> m (Either Text CanonicalPath)
canonicalPathE' = canonicalPathE . fromText

-- | Convert @CanonicalPath@ to @Filesystem.FilePath@.
--
-- /Since 0.1.0.0/
unsafePath :: CanonicalPath -> FilePath
unsafePath (CanonicalPath up) = up

-- * Functions used for canonicalization

canonicalize :: MonadIO m => FilePath -> m (Either Text FilePath)
canonicalize fp = extractPath fp >>= either (return . Left) canonicalize'

-- we do want exceptions from canonicalizePath
-- also canonicalizePath will throw exception
-- when @path does not exist
canonicalize' :: MonadIO m => Text -> m (Either Text FilePath)
canonicalize' path =
  liftIO $ liftM (right fromPrelude) (tryIO . canonicalizePath . textToString $ path)

extractPath :: MonadIO m => FilePath -> m (Either Text Text)
extractPath = liftM concatPath . mapM (extractAtom . toTextUnsafe) . splitDirectories . collapse

extractAtom :: MonadIO m => Text -> m (Either Text Text)
extractAtom atom = tryEnvPosix <||> tryHome <||> tryEnvWindows <%> atom

-- * Parsers and parser combinators

type Parser m = Text -> Maybe (m (Either Text Text))

tryEnvPosix :: MonadIO m => Parser m
tryEnvPosix x = when' (Text.isPrefixOf "$" x) (Just . getEnv . Text.tail $ x)

tryEnvWindows :: MonadIO m => Parser m
tryEnvWindows x =
  when' (Text.isPrefixOf "%" x &&
         Text.isSuffixOf "%" x)
        (Just . getEnv . Text.tail . Text.init $ x)

tryHome :: MonadIO m => Parser m
tryHome x = when' ("~" == x) (Just $ liftM Right homeDirectory)

(<||>) :: MonadIO m => Parser m -> Parser m -> Parser m
p1 <||> p2 = \v -> p1 v <|> p2 v

(<%>) :: MonadIO m => Parser m -> Text -> m (Either Text Text)
p <%> v = fromMaybe (return . Right $ v) (p v)

-- * File operations

-- | @'readFile' file@ function reads a /file/ and returns the contents of the /file/ as a @'Text'@. The /file/ is read lazily, on demand, as with getContents.
--
-- /Since 0.1.1.0/
readFile :: MonadIO m => CanonicalPath -> m Text
readFile = liftIO . BasicPrelude.readFile . unsafePath

-- | @'writeFile' file txt@ writes /txt/ to the /file/.
--
-- /Since 0.1.1.0/
writeFile :: MonadIO m => CanonicalPath -> Text -> m ()
writeFile p = liftIO . BasicPrelude.writeFile (unsafePath p)

-- | @'writeFile'' dir file txt@ writes /txt/ to the /dir\/file/. Useful, when the file isn't created yet or you don't sure if it exists.
--
-- /Since 0.1.2.0/
writeFile' :: MonadIO m => CanonicalPath -> FilePath -> Text -> m ()
writeFile' cp file = liftIO . BasicPrelude.writeFile (unsafePath cp </> file)

-- | @'appendFile' file txt@ appends /txt/ to the /file/.
--
-- /Since 0.1.1.0/
appendFile :: MonadIO m => CanonicalPath -> Text -> m ()
appendFile p = liftIO . BasicPrelude.appendFile (unsafePath p)

-- * Utilities

tryIO :: MonadIO m => IO a -> m (Either Text a)
tryIO a = liftM (left show) (try' a)
  where try' :: MonadIO m => IO a -> m (Either IOException a)
        try' = liftIO . try

getEnv :: MonadIO m => Text -> m (Either Text Text)
getEnv = liftM (right fromString) . tryIO . SE.getEnv . textToString

homeDirectory :: MonadIO m => m Text
homeDirectory = liftIO $ fromString <$> getHomeDirectory

when' :: Alternative f => Bool -> f a -> f a
when' b v = if b then v else Applicative.empty

concatPath :: [Either Text Text] -> Either Text Text
-- concatPath = (right BasicPrelude.concat) . sequence
concatPath = (right (("/" ++) . Text.intercalate "/")) . sequence . tail

preludeMap :: (Prelude.FilePath -> a) -> CanonicalPath -> a
preludeMap f = f . toPrelude . unsafePath

-- | @'toText' path@ converts 'FilePath' /path/ to 'Text'. In case of any problems it will throw error.
--
-- See 'Filesystem.Path.CurrentOS.toText' function for details.
--
-- /Since 0.3.0.0/
toTextUnsafe :: FilePath -> Text
toTextUnsafe = either (error . textToString) id . toText

-- | @'toText'' path@ converts 'CanonicalPath' to 'Text'.
--
-- /Since 0.3.0.0/
toText' :: CanonicalPath -> Text
toText' = toTextUnsafe . unsafePath

-- | @'fromPrelude' fp'@ converts 'Prelude.FilePath' to 'FilePath'.
--
-- /Since 0.1.0.0/
fromPrelude :: Prelude.FilePath -> FilePath
fromPrelude = fromText . Text.pack

-- | @'toPrelude' up'@ converts 'FilePath' to 'Prelude.FilePath'.
--
-- /Since 0.1.0.0/
toPrelude :: FilePath -> Prelude.FilePath
toPrelude = Text.unpack . toTextUnsafe

voidM :: Monad m => m a -> m ()
voidM a = a >> return ()

{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Internal (CanonicalPath(..)
                                         ,canonicalPath
                                         ,canonicalPath'
                                         ,canonicalPathM
                                         ,canonicalPathM'
                                         ,canonicalPathE
                                         ,canonicalPathE'
                                         ,unsafePath
                                         ,UnsafePath
                                         ,SafePath
                                         ,Filesystem.CanonicalPath.Internal.readFile
                                         ,Filesystem.CanonicalPath.Internal.writeFile
                                         ,writeFile'
                                         ,Filesystem.CanonicalPath.Internal.appendFile
                                         ,preludeMap
                                         ,pathToText
                                         ,textToPath
                                         ,cpathToText
                                         ,toPrelude
                                         ,fromPrelude
                                         ,addSlash
                                         ,voidM) where

import           BasicPrelude
import           Control.Applicative as Applicative
import           Control.Arrow (left)
import           Data.Text ()
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Prelude
import           System.Directory (getHomeDirectory
                                  ,canonicalizePath
                                  ,doesDirectoryExist
                                  ,doesFileExist)
import qualified System.Environment as SE (getEnv)

newtype CanonicalPath = CanonicalPath UnsafePath

instance Show CanonicalPath where
  showsPrec d path =
    showParen (d > 15)
              (showString "CanonicalPath " .
               shows (toText path))
    where toText (CanonicalPath p) = pathToText p

{-|
Unsafe constructor of @CanonicalPath@. In case of any problem it will @error@.

Example:

>>> canonicalPath "$HOME"
CanonicalPath "/Users/your-user-name"

>>> canonicalPath "unknown"
*** Exception: Path does not exist (no such file or directory): unknown

/Since 0.1.0.0/
-}
canonicalPath :: MonadIO m => UnsafePath -> m CanonicalPath
canonicalPath path = canonicalize path >>= either (error . textToString) (return . CanonicalPath)

{-|
Version of @canonicalPath@ that takes @Text@ instead of @UnsafePath@.

/Since 0.2.1.0/
-}
canonicalPath' :: MonadIO m => Text -> m CanonicalPath
canonicalPath' = canonicalPath . textToPath

{-|
Constructs @Maybe CanonicalPath@.

>>> canonicalPathM "~"
Just CanonicalPath "Users/your-user-name"

>>> canonicalPathM "unknown"
Nothing

/Since 0.1.0.0/
-}
canonicalPathM :: MonadIO m => UnsafePath -> m (Maybe CanonicalPath)
canonicalPathM path = canonicalize path >>= either (\_ -> return Nothing) (return . Just . CanonicalPath)

{-|
Version of @canonicalPathM@ that takes @Text@ instead of @UnsafePath@.

/Since 0.2.1.0/
-}
canonicalPathM' :: MonadIO m => Text -> m (Maybe CanonicalPath)
canonicalPathM' = canonicalPathM . textToPath

{-|
Constructs @Either Text CanonicalPath@.

>>> canonicalPathE "~/"
Right CanonicalPath "/Users/your-user-name"

>>> canonicalPathE "$HOME/this-folder-does-not-exist"
Left "Path does not exist (no such file or directory): /Users/your-user-name/this-folder-does-not-exist"

/Since 0.1.0.0/
-}
canonicalPathE :: MonadIO m => UnsafePath -> m (Either Text CanonicalPath)
canonicalPathE path = canonicalize path >>= either (return . Left) (return . Right . CanonicalPath)

{-|
Version of @canonicalPathE@ that takes @Text@ instead of @UnsafePath@.

/Since 0.2.1.0/
-}
canonicalPathE' :: MonadIO m => Text -> m (Either Text CanonicalPath)
canonicalPathE' = canonicalPathE . textToPath

-- | Convert @CanonicalPath@ to @Filesystem.FilePath@.
--
-- /Since 0.1.0.0/
unsafePath :: CanonicalPath -> UnsafePath
unsafePath (CanonicalPath up) = up

-- | Synonym of @FilePath@ from @Filesystem.Path@ module.
--
-- /Since 0.1.0.0/
type UnsafePath = FilePath.FilePath
type SafePath = Either Text UnsafePath

-- * Functions used for canonicalization

canonicalize :: MonadIO m => UnsafePath -> m SafePath
canonicalize fp = extractPath fp >>= either (return . Left) canonicalize'

canonicalize' :: MonadIO m => UnsafePath -> m SafePath
canonicalize' fp =
  do exists <- liftIO $ liftM2 (||) (doesFileExist . toPrelude $ fp) (doesDirectoryExist . toPrelude $ fp)
     if exists
        then liftIO $ liftM Right (pathMap canonicalizePath fp)
        else return . Left $ "Path does not exist (no such file or directory): " ++ pathToText fp

extractPath :: MonadIO m => UnsafePath -> m SafePath
extractPath = liftM concatPath . mapM extractAtom . FilePath.splitDirectories

extractAtom :: MonadIO m => UnsafePath -> m SafePath
extractAtom atom = tryEnvPosix <||> tryEnvWindows <||> tryHome <%> atom

-- * Parsers and parser combinators

type Parser m = UnsafePath -> Maybe (m SafePath)

tryEnvPosix :: MonadIO m => Parser m
tryEnvPosix x = when' (hasPrefix "$" x) (Just . getEnv . pathTail $ x)

tryEnvWindows :: MonadIO m => Parser m
tryEnvWindows x =
  when' (hasPrefix "%" x &&
         hasSuffix "%" x)
        (Just . getEnv . pathTail . pathInit $ x)

tryHome :: MonadIO m => Parser m
tryHome x = when' (textToPath "~" == x) (Just $ liftM Right homeDirectory)

(<||>) :: MonadIO m => Parser m -> Parser m -> Parser m
p1 <||> p2 = \v -> p1 v <|> p2 v

(<%>) :: MonadIO m => Parser m -> UnsafePath -> m SafePath
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
writeFile' :: MonadIO m => CanonicalPath -> UnsafePath -> Text -> m ()
writeFile' cp file = liftIO . BasicPrelude.writeFile (unsafePath cp </> file)

-- | @'appendFile' file txt@ appends /txt/ to the /file/.
--
-- /Since 0.1.1.0/
appendFile :: MonadIO m => CanonicalPath -> Text -> m ()
appendFile p = liftIO . BasicPrelude.appendFile (unsafePath p)

-- * Utilities

getEnv :: MonadIO m => UnsafePath -> m SafePath
getEnv var = liftM (left show) tryEnv
  where env = pathMap SE.getEnv
        tryEnv :: MonadIO m => m (Either IOException UnsafePath)
        tryEnv = liftIO . try . env $ var

homeDirectory :: MonadIO m => m UnsafePath
homeDirectory = liftIO $ fromPrelude <$> getHomeDirectory

when' :: Alternative f => Bool -> f a -> f a
when' b v = if b then v else Applicative.empty

pathMap :: MonadIO m => (Prelude.FilePath -> m Prelude.FilePath) -> UnsafePath -> m UnsafePath
pathMap f p = liftM fromPrelude (f . toPrelude $ p)

hasPrefix :: Text -> UnsafePath -> Bool
hasPrefix prefix path = prefix `Text.isPrefixOf` pathToText path

hasSuffix :: Text -> UnsafePath -> Bool
hasSuffix suffix path = suffix `Text.isSuffixOf` pathToText path

pathTail :: UnsafePath -> UnsafePath
pathTail = textToPath . Text.tail . pathToText

pathInit :: UnsafePath -> UnsafePath
pathInit = textToPath . Text.init . pathToText

addSlash :: UnsafePath -> UnsafePath
addSlash = textToPath . (++ "/") . pathToText

concatPath :: [SafePath] -> SafePath
concatPath = foldl' (<//>) (Right "")

(<//>) :: SafePath -> SafePath -> SafePath
(<//>) l@(Left _) _ = l
(<//>) _ l@(Left _) = l
(<//>) (Right a) (Right b) = Right $ a </> b

preludeMap :: (Prelude.FilePath -> a) -> CanonicalPath -> a
preludeMap f = f . toPrelude . unsafePath

-- * Type conversions

-- | @'pathToText' path@ converts 'UnsafePath' /path/ to 'Text'. In case of eny problems it will throw error.
--
-- See 'Filesystem.Path.CurrentOS.toText' function for details.
--
-- /Since 0.1.2.0/
pathToText :: UnsafePath -> Text
pathToText = either (error . textToString) id . FilePath.toText

-- | @'textToPath' txt@ converts 'Text' to 'UnsafePath'.
--
-- /Since 0.1.2.0/
textToPath :: Text -> UnsafePath
textToPath = FilePath.fromText

-- | @'cpathToText' path@ converts 'CanonicalPath' to 'Text'.
--
-- /Since 0.2.3.0/
cpathToText :: CanonicalPath -> Text
cpathToText = pathToText . unsafePath

-- | @'fromPrelude' fp'@ converts 'Prelude.FilePath' to 'UnsafePath'.
--
-- /Since 0.1.0.0/
fromPrelude :: Prelude.FilePath -> UnsafePath
fromPrelude = textToPath . Text.pack

-- | @'toPrelude' up'@ converts 'UnsafePath' to 'Prelude.FilePath'.
--
-- /Since 0.1.0.0/
toPrelude :: UnsafePath -> Prelude.FilePath
toPrelude = Text.unpack . pathToText

voidM :: Monad m => m a -> m ()
voidM a = a >> return ()

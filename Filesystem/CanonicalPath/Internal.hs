{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Internal (CanonicalPath(..)
                                         ,canonicalPath
                                         ,canonicalPathM
                                         ,canonicalPathE
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
                                         ,toPrelude
                                         ,fromPrelude
                                         ,addSlash) where

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

data CanonicalPath = CanonicalPath UnsafePath

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
canonicalPath :: UnsafePath -> IO CanonicalPath
canonicalPath path = canonicalize path >>= either (error . textToString) (return . CanonicalPath)

{-|
Constructs @Maybe CanonicalPath@.

>>> canonicalPathM "~"
Just CanonicalPath "Users/your-user-name"

>>> canonicalPathM "unknown"
Nothing

/Since 0.1.0.0/
-}
canonicalPathM :: UnsafePath -> IO (Maybe CanonicalPath)
canonicalPathM path = canonicalize path >>= either (\_ -> return Nothing) (return . Just . CanonicalPath)

{-|
Constructs @Either Text CanonicalPath@.

>>> canonicalPathE "~/"
Right CanonicalPath "/Users/your-user-name"

>>> canonicalPathE "$HOME/this-folder-does-not-exist"
Left "Path does not exist (no such file or directory): /Users/your-user-name/this-folder-does-not-exist"

/Since 0.1.0.0/
-}
canonicalPathE :: UnsafePath -> IO (Either Text CanonicalPath)
canonicalPathE path = canonicalize path >>= either (return . Left) (return . Right . CanonicalPath)

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

canonicalize :: UnsafePath -> IO SafePath
canonicalize fp = extractPath fp >>= either (return . Left) canonicalize'

canonicalize' :: UnsafePath -> IO SafePath
canonicalize' fp =
  do exists <- liftM2 (||) (doesFileExist . toPrelude $ fp) (doesDirectoryExist . toPrelude $ fp)
     if exists
        then liftM Right (pathMap canonicalizePath fp)
        else return . Left $ "Path does not exist (no such file or directory): " ++ pathToText fp

extractPath :: UnsafePath -> IO SafePath
extractPath = liftM concatPath . mapM extractAtom . FilePath.splitDirectories

extractAtom :: UnsafePath -> IO SafePath
extractAtom atom = tryEnvPosix <||> tryEnvWindows <||> tryHome <%> atom

-- * Parsers and parser combinators

type Parser = UnsafePath -> Maybe (IO SafePath)

tryEnvPosix :: Parser
tryEnvPosix x = when' (hasPrefix "$" x) (Just . getEnv . pathTail $ x)

tryEnvWindows :: Parser
tryEnvWindows x =
  when' (hasPrefix "%" x &&
         hasSuffix "%" x)
        (Just . getEnv . pathTail . pathInit $ x)

tryHome :: Parser
tryHome x = when' (textToPath "~" == x) (Just $ liftM Right homeDirectory)

(<||>) :: Parser -> Parser -> Parser
p1 <||> p2 = \v -> p1 v <|> p2 v

(<%>) :: Parser -> UnsafePath -> IO SafePath
p <%> v = fromMaybe (return . Right $ v) (p v)

-- * File operations

-- | @'readFile' file@ function reads a /file/ and returns the contents of the /file/ as a @'Text'@. The /file/ is read lazily, on demand, as with getContents.
--
-- /Since 0.1.1.0/
readFile :: CanonicalPath -> IO Text
readFile = BasicPrelude.readFile . unsafePath

-- | @'writeFile' file txt@ writes /txt/ to the /file/.
--
-- /Since 0.1.1.0/
writeFile :: CanonicalPath -> Text -> IO ()
writeFile = BasicPrelude.writeFile . unsafePath

-- | @'writeFile'' dir file txt@ writes /txt/ to the /dir\/file/. Useful, when the file isn't created yet or you don't sure if it exists.
--
-- /Since 0.1.2.0/
writeFile' :: CanonicalPath -> UnsafePath -> Text -> IO ()
writeFile' cp file = BasicPrelude.writeFile (unsafePath cp </> file)

-- | @'appendFile' file txt@ appends /txt/ to the /file/.
--
-- /Since 0.1.1.0/
appendFile :: CanonicalPath -> Text -> IO ()
appendFile = BasicPrelude.appendFile . unsafePath

-- * Utilities

getEnv :: UnsafePath -> IO SafePath
getEnv var = map (left show) tryEnv
  where env = pathMap SE.getEnv
        tryEnv :: IO (Either IOException UnsafePath)
        tryEnv = try . env $ var

homeDirectory :: IO UnsafePath
homeDirectory = fromPrelude <$> getHomeDirectory

when' :: Alternative f => Bool -> f a -> f a
when' b v = if b then v else Applicative.empty

pathMap :: (Prelude.FilePath -> IO Prelude.FilePath) -> UnsafePath -> IO UnsafePath
pathMap f = map fromPrelude . f . toPrelude

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

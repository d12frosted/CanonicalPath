{-|
Module      : Filesystem.CanonicalPath.Directory
Copyright   : (c) Boris Buliga, 2014
License     : MIT
Maintainer  : d12frosted@icloud.com
Stability   : experimental
Portability : portable

Redefinition of some functions from @System.Directory@ module. Some of them have different signature, because they need to work with @'CanonicalPath'@. For example, we can't create functions @createDirectory :: 'CanonicalPath' -> IO ()@, because it has no sense. How can we create directory that already exists? Instead we have function @createDirectory :: 'CanonicalPath' -> 'UnsafePath' -> IO 'CanonicalPath'@, that creates new directory in base existing directory with provided name. And also it returns @'CanonicalPath'@ of newly created directory. Isn't it nice?

Happy Haskell Hacking!
-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Directory where

import           BasicPrelude
import           Data.Text ()
import           Filesystem.CanonicalPath
import           Filesystem.CanonicalPath.Internal
import           Filesystem.Path
import           Prelude ()
import qualified System.Directory as Directory

{-|
@'createDirectory' base dir@ creates new directory @dir@ in existing @base@ directory and returns @'CanonicalPath'@ of created directory.

For more information look for documentation of @'System.Directory.createDirectory'@.

/Since 0.1.1.0/
-}
createDirectory :: CanonicalPath -- ^ base directory
                -> UnsafePath -- ^ name of new directory
                -> IO CanonicalPath -- ^ @'CanonicalPath'@ of created directory
createDirectory cp dir =
  do Directory.createDirectory $ toPrelude path
     return $ CanonicalPath path
  where path = unsafePath cp </> dir

{-|
@'createDirectoryIfMissing' parents dir@ creates a new directory @dir@ in @base@ directory. If the first argument is 'True' the function will also create all parent directories if they are missing. Function returns @'CanonicalPath'@ of created directory.

For more information look for documentation of @'System.Directory.createDirectoryIfMissing'@.

/Since 0.1.1.0/
-}
createDirectoryIfMissing :: Bool -- ^ Create its parents too?
                         -> CanonicalPath -- ^ base directory
                         -> UnsafePath -- ^ name of new directory
                         -> IO CanonicalPath -- ^ @'CanonicalPath'@ of created directory
createDirectoryIfMissing flag cp dir =
  do Directory.createDirectoryIfMissing flag $ toPrelude path
     return $ CanonicalPath path
  where path = unsafePath cp </> dir

{-|
@'removeDirectory' dir@ removes an existing directory /dir/.

For more information look for documentation of @'System.Directory.removeDirectory'@.

/Since 0.1.1.0/
-}
removeDirectory :: CanonicalPath -> IO ()
removeDirectory = preludeMap Directory.removeDirectory

{-|
@'removeDirectoryRecursive' dir@  removes an existing directory /dir/ together with its content and all subdirectories. Be careful, if the directory contains symlinks, the function will follow them.

For more information look for documentation of @'System.Directory.removeDirectoryRecursive'@.

/Since 0.1.1.0/
-}
removeDirectoryRecursive :: CanonicalPath -> IO ()
removeDirectoryRecursive = preludeMap Directory.removeDirectoryRecursive

{-|
@'renameDirectory' old new@ changes the name of an existing directory from /old/ to /new/ and returns @'CanonicalPath'@ of new directory.

For more information look for documentation of @'System.Directory.renameDirectory'@.

/Since 0.1.1.0/
-}
renameDirectory :: CanonicalPath -- ^ old directory
                -> UnsafePath -- ^ new directory (should be just name of directory)
                -> IO CanonicalPath -- ^ @'CanonicalPath'@ of new directory
renameDirectory cp p =
  do newPath <- canonicalPath $ parent p
     Directory.renameDirectory (toPrelude . unsafePath $ cp) (toPrelude p)
     return . CanonicalPath $ unsafePath newPath </> dirname (addSlash p)

{-|
@'getDirectoryContents' dir@ returns a list of /all/ entries in /dir/. If you want to have list of @'CanonicalPath'@ instead use function @'getDirectoryContents''@.

For more information look for documentation of @'System.Directory.getDirectoryContents'@.

/Since 0.1.1.0/
-}
getDirectoryContents :: CanonicalPath -> IO [UnsafePath]
getDirectoryContents cp = liftM (fromPrelude <$>) $ preludeMap Directory.getDirectoryContents cp

{-|
The same as @'getDirectoryContents'@, but returns list of @'CanonicalPath'@ instead of @'UnsafePath'@.

/Since 0.1.1.0/
-}
getDirectoryContents' :: CanonicalPath -> IO [CanonicalPath]
getDirectoryContents' cp = liftM (CanonicalPath . (</> ) up <$>) $ getDirectoryContents cp
  where up = unsafePath cp

{- |If the operating system has a notion of current directories, 'getCurrentDirectory' returns an @'CanonicalPath'@ to the current directory of the calling process.

For more information look for documentation of @'System.Directory.getCurrentDirectory'@.

/Since 0.1.1.0/
-}
getCurrentDirectory :: IO CanonicalPath
getCurrentDirectory = liftM (CanonicalPath . fromPrelude) Directory.getCurrentDirectory

{-|
If the operating system has a notion of current directories, @'setCurrentDirectory' dir@ changes the current directory of the calling process to /dir/.

For more information look for documentation of @'System.Directory.setCurrentDirectory'@.

/Since 0.1.1.0/
-}
setCurrentDirectory :: CanonicalPath -> IO ()
setCurrentDirectory = preludeMap Directory.setCurrentDirectory

{-|
Returns the current user's home directory.

For more information look for documentation of @'System.Directory.getHomeDirectory'@.

/Since 0.1.1.0/
-}
getHomeDirectory :: IO CanonicalPath
getHomeDirectory = liftM (CanonicalPath . fromPrelude) Directory.getHomeDirectory

{-|
Returns the @'CanonicalPath'@ of a directory in which application-specific data for the current user can be stored.  The result of 'getAppUserDataDirectory' for a given application is specific to the current user.

For more information look for documentation of @'System.Directory.getAppUserDataDirectory'@.

/Since 0.1.1.0/
-}
getAppUserDataDirectory :: Text -> IO UnsafePath
getAppUserDataDirectory = liftM fromPrelude . Directory.getAppUserDataDirectory . textToString

{-|
Returns the current user's document directory.

For more information look for documentation of @'System.Directory.getUserDocumentsDirectory'@.

/Since 0.1.1.0/
-}
getUserDocumentsDirectory :: IO CanonicalPath
getUserDocumentsDirectory = liftM (CanonicalPath . fromPrelude) Directory.getUserDocumentsDirectory

{-|
Returns the current directory for temporary files.

For more information look for documentation of @'System.Directory.getUserDocumentsDirectorygetTemporaryDirectory'@.

/Since 0.1.1.0/
-}
getTemporaryDirectory :: IO UnsafePath
getTemporaryDirectory = liftM fromPrelude Directory.getTemporaryDirectory

{-|
'removeFile' /file/ removes the directory entry for an existing file /file/, where /file/ is not itself a directory.

For more information look for documentation of @'System.Directory.removeFile'@.

/Since 0.1.1.0/
-}
removeFile :: CanonicalPath -> IO ()
removeFile = preludeMap Directory.removeFile

{-|
@'renameFile' old new@ changes the name of an existing file system object from /old/ to /new/.

For more information look for documentation of @'System.Directory.renameFile'@.

/Since 0.1.1.0/
-}
renameFile :: CanonicalPath -- ^ @'CanonicalPath'@ of file you want to rename
           -> UnsafePath -- ^ new name of file
           -> IO CanonicalPath -- ^ @'CanonicalPath'@ of /new/ file
renameFile cp p =
  do newPath <- canonicalPath $ parent p
     Directory.renameFile (toPrelude . unsafePath $ cp) (toPrelude p)
     return . CanonicalPath $ unsafePath newPath </> filename p

{-|
@'copyFile' old new@ copies the existing file from /old/ to /new/. If the /new/ file already exists, it is atomically replaced by the /old/ file. Neither path may refer to an existing directory.  The permissions of /old/ are copied to /new/, if possible.

For more information look for documentation of @'System.Directory.copyFile'@.

/Since 0.1.1.0/
-}
copyFile :: CanonicalPath -- ^ @'CanonicalPath'@ of file you want to copy
         -> UnsafePath -- ^ name of new file (actually it can be path relative to directory of /old/
         -> IO CanonicalPath -- ^ @'CanonicalPath'@ of /new/ file
copyFile oldFile newFile =
  do newPath <- canonicalPath $ parent newFile
     Directory.copyFile (toPrelude . unsafePath $ oldFile) (toPrelude newFile)
     return . CanonicalPath $ unsafePath newPath </> filename newFile

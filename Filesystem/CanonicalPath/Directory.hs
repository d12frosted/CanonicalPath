{-# LANGUAGE OverloadedStrings #-}

module Filesystem.CanonicalPath.Directory where

import           BasicPrelude
import           Data.Text ()
import           Filesystem.CanonicalPath
import           Filesystem.CanonicalPath.Internal
import           Filesystem.Path
import           Prelude ()
import qualified System.Directory as Directory

createDirectory :: CanonicalPath -> UnsafePath -> IO CanonicalPath
createDirectory cp dir =
  do Directory.createDirectory $ toPrelude path
     return $ CanonicalPath path
  where path = unsafePath cp </> dir

createDirectoryIfMissing :: Bool -> CanonicalPath -> UnsafePath -> IO CanonicalPath
createDirectoryIfMissing flag cp dir =
  do Directory.createDirectoryIfMissing flag $ toPrelude path
     return $ CanonicalPath path
  where path = unsafePath cp </> dir

removeDirectory :: CanonicalPath -> IO ()
removeDirectory = preludeMap Directory.removeDirectory

removeDirectoryRecursive :: CanonicalPath -> IO ()
removeDirectoryRecursive = preludeMap Directory.removeDirectoryRecursive

renameDirectory :: CanonicalPath -> UnsafePath -> IO CanonicalPath
renameDirectory cp p =
  do newPath <- canonicalPath $ parent p
     Directory.renameDirectory (toPrelude . unsafePath $ cp) (toPrelude p)
     return . CanonicalPath $ unsafePath newPath </> dirname (addSlash p)

getDirectoryContents :: CanonicalPath -> IO [UnsafePath]
getDirectoryContents cp = liftM (fromPrelude <$>) $ preludeMap Directory.getDirectoryContents cp

getDirectoryContents' :: CanonicalPath -> IO [CanonicalPath]
getDirectoryContents' cp = liftM (CanonicalPath . (</> ) up <$>) $ getDirectoryContents cp
  where up = unsafePath cp

getCurrentDirectory :: IO CanonicalPath
getCurrentDirectory = liftM (CanonicalPath . fromPrelude) Directory.getCurrentDirectory

setCurrentDirectory :: CanonicalPath -> IO ()
setCurrentDirectory = preludeMap Directory.setCurrentDirectory

getHomeDirectory :: IO CanonicalPath
getHomeDirectory = liftM (CanonicalPath . fromPrelude) Directory.getHomeDirectory

getAppUserDataDirectory :: Text -> IO UnsafePath
getAppUserDataDirectory = liftM fromPrelude . Directory.getAppUserDataDirectory . textToString

getUserDocumentsDirectory :: IO CanonicalPath
getUserDocumentsDirectory = liftM (CanonicalPath . fromPrelude) Directory.getUserDocumentsDirectory

getTemporaryDirectory :: IO CanonicalPath
getTemporaryDirectory = liftM (CanonicalPath . fromPrelude) Directory.getTemporaryDirectory

removeFile :: CanonicalPath -> IO ()
removeFile = preludeMap Directory.removeFile

renameFile :: CanonicalPath -> UnsafePath -> IO CanonicalPath
renameFile cp p =
  do newPath <- canonicalPath $ parent p
     Directory.renameFile (toPrelude . unsafePath $ cp) (toPrelude p)
     return . CanonicalPath $ unsafePath newPath </> filename p

copyFile :: CanonicalPath -> UnsafePath -> IO CanonicalPath
copyFile oldFile newFile =
  do newPath <- canonicalPath $ parent newFile
     Directory.copyFile (toPrelude . unsafePath $ oldFile) (toPrelude newFile)
     return . CanonicalPath $ unsafePath newPath </> filename newFile

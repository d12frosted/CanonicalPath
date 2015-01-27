{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           BasicPrelude
import qualified Control.Arrow as Arrow(right)
import           Filesystem.CanonicalPath
import           Filesystem.CanonicalPath.Directory
import           Test.Chell

main :: IO ()
main = defaultMain [testCanonicalPath]

testCanonicalPath :: Suite
testCanonicalPath =
  suite "canonical path constructors" $
  [testConstructor]

testConstructor :: Test
testConstructor =
  assertions "canonicalPathE tests" $
  do $expect $ equal' (canonicalize "dir/") (good "dir")
     $expect $ equal' (canonicalize "file") (good "file")
     $expect $ equal' (canonicalize "file/") (good "file")
     $expect $ equal' (canonicalize "dir/file1.txt") (good "dir/file1.txt")
     $expect $ equal' (canonicalize "dir/ab/") (good "dir/ab")
     $expect $ equal' (canonicalize "dir/file") (good "dir/file")
     $expect $ equal' (canonicalize "dir/ab/../file") (good "dir/file")
     $expect $ equal' (canonicalize "dir/ab/../ab/") (good "dir/ab")
     $expect $ equal' (canonicalize "dir/ab/./file2.txt") (good "dir/ab/file2.txt")
     $expect $ equal' (canonicalize "dir/ab/../../dir/ab/./../file1.txt") (good "dir/file1.txt")
     $expect $ equal' (canonicalize deepFile1) (good deepFile1)
     $expect $ equal' (canonicalize "diro/") (bad "diro" notExistError)
     $expect $ equal' (canonicalize "dir.txt") (bad "dir.txt" notExistError)
     $expect $ equal' (canonicalize "dir/ab/../../dir/ab/./../file1.tx") (bad "dir/file1.tx" notExistError)
     $expect $ equal' (canonicalize deepFile2) (bad deepFile2 pathToLongError)

-- Helper functions

canonicalize :: FilePath -> IO (Either Text FilePath)
canonicalize p =
  do h <- currentDir
     liftM (Arrow.right unsafePath) . canonicalPathE $ (h </> p)

good :: FilePath -> IO (Either Text FilePath)
good p =
  do h <- currentDir
     return . Right $ (h </> p)

bad :: FilePath -> Text -> IO (Either Text FilePath)
bad p e =
  do h <- currentDir
     return . Left $ (either id id . toText $ h </> p) ++ e

equal' :: (Show a, Eq a) => IO a -> IO a -> IO Assertion
equal' a b =
  do a' <- a
     b' <- b
     return $ equal a' b'

currentDir :: IO FilePath
currentDir = liftM ((</> "tests") . unsafePath) getCurrentDirectory

deepFile1 :: FilePath
deepFile1 = "dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-file.txt"

deepFile2 :: FilePath
deepFile2 = "dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-dir/deep-file.txt"

notExistError :: Text
notExistError = ": canonicalizePath: does not exist (No such file or directory)"

pathToLongError :: Text
pathToLongError = ": canonicalizePath: invalid argument (File name too long)"

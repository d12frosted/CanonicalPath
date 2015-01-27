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
  do let left' v = v >>= \v' -> return $ left v'
         right' v = v >>= \v' -> return $ right v'

     $expect $ right' (canonicalize "dir/")
     $expect $ right' (canonicalize "file")
     $expect $ right' (canonicalize "file/")
     $expect $ right' (canonicalize "dir/file1.txt")
     $expect $ right' (canonicalize "dir/ab/")
     $expect $ right' (canonicalize "dir/file")
     $expect $ right' (canonicalize "dir/ab/../file")
     $expect $ right' (canonicalize "dir/ab/../ab/")
     $expect $ right' (canonicalize "dir/ab/./file2.txt")
     $expect $ right' (canonicalize "dir/ab/../../dir/ab/./../file1.txt")
     $expect $ left' (canonicalize "diro/")
     $expect $ left' (canonicalize "dir.txt")
     $expect $ left' (canonicalize "dir/ab/../../dir/ab/./../file1.tx")
     $expect $ left' (canonicalize deepFile)

-- Helper functions

canonicalize :: FilePath -> IO (Either Text FilePath)
canonicalize p =
  do h <- currentDir
     liftM (Arrow.right unsafePath) . canonicalPathE $ (h </> p)

currentDir :: IO FilePath
currentDir = liftM ((</> "tests") . unsafePath) getCurrentDirectory

deepFile :: FilePath
deepFile = concat $ replicate 500 "deep-hell"

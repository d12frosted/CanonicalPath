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
         equal' v w = do
           v' <- v
           w' <- w
           return $ equal v' w'
         check p1 p2 = equal' (canonicalize' p1) (liftM Right $ inCurrentDir p2)

     $expect $ left' (canonicalize' "diro/")
     $expect $ left' (canonicalize' "dir.txt")
     $expect $ left' (canonicalize' "dir/ab/../../dir/ab/./../file1.tx")
     $expect $ left' (canonicalize' deepFile)

     $expect $ check "dir/" "dir"
     $expect $ check "dir" "dir"
     $expect $ check "file" "file"
     -- check "file/" "file" will fail on linux
     -- but will not fail on os x
     $expect $ check "dir/file1.txt" "dir/file1.txt"
     $expect $ check "dir/ab/" "dir/ab"
     $expect $ check "dir/file" "dir/file"
     $expect $ check "dir/ab/../file" "dir/file"
     $expect $ check "dir/ab/../ab/" "dir/ab"
     $expect $ check "dir/ab/./file2.txt" "dir/ab/file2.txt"
     $expect $ check "dir/ab/../../dir/ab/./../file1.txt" "dir/file1.txt"

     $expect $ equal' (canonicalize "$HOME") (liftM Right getHomeDirectory)
     $expect $ equal' (canonicalize "$HOME/") (liftM Right getHomeDirectory)
     $expect $ equal' (canonicalize "~/") (liftM Right getHomeDirectory)

-- Helper functions

canonicalize :: FilePath -> IO (Either Text CanonicalPath)
canonicalize = canonicalPathE

canonicalize' :: FilePath -> IO (Either Text FilePath)
canonicalize' p = inCurrentDir p >>= liftM (Arrow.right unsafePath) . canonicalize

currentDir :: IO FilePath
currentDir = liftM ((</> "tests") . unsafePath) getCurrentDirectory

inCurrentDir :: FilePath -> IO FilePath
inCurrentDir p =
  do h <- currentDir
     return $ h </> p

deepFile :: FilePath
deepFile = concat $ replicate 500 "deep-hell"

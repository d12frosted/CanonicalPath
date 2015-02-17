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
main =
  do createTestFiles
     defaultMain [testCanonicalPath]

createTestFiles :: IO ()
createTestFiles =
  do r <- canonicalPathE rootdir
     either (\_ -> return ()) removeDirectoryRecursive r
     wd <- getCurrentDirectory
     root <- createDirectory wd rootdir
     d1 <- createDirectory root "dir"
     d2 <- createDirectory d1 "ab"
     createDirectory_ root "file-or-dir"
     writeFile' root "file" ""
     writeFile' d1 "file" ""
     writeFile' d1 "file1.txt" ""
     writeFile' d2 "file2.txt" ""
     return ()

testCanonicalPath :: Suite
testCanonicalPath =
  suite "canonical path constructors" $
  [testLeftSide
  ,testRightSide
  ,testVariables]

testLeftSide :: Test
testLeftSide =
  assertions "check for expected lefts" $
  do $expect $ left' (mkPath' "diro/")
     $expect $ left' (mkPath' "dir.txt")
     $expect $ left' (mkPath' "dir/ab/../../dir/ab/./../file1.tx")
     $expect $ left' (mkPath' deepFile)

testRightSide :: Test
testRightSide =
  assertions "check for expected rights" $
  do $expect $ check "dir/" "dir"
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

testVariables :: Test
testVariables =
  assertions "check for extracting environment variables" $
  do $expect $ equal' (mkPath "$HOME") (mkPath "~/")
     $expect $ right' (mkPath "$TMPDIR")

-- Chell helpers

left' :: (Show b, Monad m) => m (Either a b) -> m Assertion
left' v = v >>= \v' -> return $ left v'

right' :: (Show a, Monad m) => m (Either a b) -> m Assertion
right' v = v >>= \v' -> return $ right v'

equal' :: (Show a, Monad m, Eq a) => m a -> m a -> m Assertion
equal' v w = do
  v' <- v
  w' <- w
  return $ equal v' w'

check :: FilePath -> FilePath -> IO Assertion
check p1 p2 = equal' (mkPath' p1) (liftM Right $ inCurrentDir p2)

-- Helper functions

mkPath :: FilePath -> IO (Either Text CanonicalPath)
mkPath = canonicalPathE

mkPath' :: FilePath -> IO (Either Text FilePath)
mkPath' p = inCurrentDir p >>= liftM unsafe . mkPath

unsafe :: Either Text CanonicalPath -> Either Text FilePath
unsafe = Arrow.right unsafePath

currentDir :: IO FilePath
currentDir = liftM ((</> rootdir) . unsafePath) getCurrentDirectory

inCurrentDir :: FilePath -> IO FilePath
inCurrentDir p =
  do h <- currentDir
     return $ h </> p

deepFile :: FilePath
deepFile = concat $ replicate 500 "deep-hell"

rootdir :: IsString a => a
rootdir = "test-root"

{-|
Module      : Filesystem.CanonicalPath
Copyright   : (c) Boris Buliga, 2014
License     : MIT
Maintainer  : d12frosted@icloud.com
Stability   : stable
Portability : portable

@Prelude.FilePath@ is just synonym for @String@, so actually it can be anything - your mothers name or path to file you want to edit. Just look at type signature of function @readFile :: FilePath -> IO String@ which can be converted to @readFile :: String -> IO String@. You can't be sure that it wont break your program in runtime.

OK, you can use @FilePath@ data type from @Filesystem.Path@ module instead of @Prelude.FilePath@. I really like this data type but it has the same problem as @Prelude.FilePath@ - you can't be sure, that it holds actual path to some file or directory on your system.

So here comes abstract type @CanonicalPath@. It solves main problem - @CanonicalPath@ points to existing file or directory. Well, to be honest, it can't guarantee that this path will refer to existing file or directory always (someone can remove or move it to another path - and it's almost impossible to be aware of such cruelty) - hopefully you can always check if @CanonicalPath@ is not /broken/ (using composition of any constructor and 'unsafePath'). But in most cases @CanonicalPath@ will help you avoid many problems and will make your routine less routine.

Oh, and last but not least, @CanonicalPath@ constructor extracts all environment variables. Also it treats @~@ as home directory.

Happy Haskell Hacking!
-}
module Filesystem.CanonicalPath
  (-- * Abstract Type
   CanonicalPath
  ,UnsafePath

  -- * Constructors
  ,canonicalPath
  ,canonicalPath'
  ,canonicalPathM
  ,canonicalPathM'
  ,canonicalPathE
  ,canonicalPathE'
  ,unsafePath

  -- * Some IO functions
  ,readFile
  ,writeFile
  ,writeFile'
  ,appendFile

  -- * Conversion functions
  ,pathToText
  ,textToPath) where

import Filesystem.CanonicalPath.Internal
import Prelude ()

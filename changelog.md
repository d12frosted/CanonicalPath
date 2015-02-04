0.3.0.0:
* remove `UnsafePath` data type. It was synonym to `FilePath` and was confusing me sometimes, so I decided to cut it off
* rename `cpathToText` to `toText'`
* remove `pathToText` and `textToPath`
* export `toText` and `fromText` from `Filesystem.Path.CurrentOS`
* export `fromPrelude` and `toPrelude` functions
* improve performance (path canonicalization now is performed 1.6x faster than before)
* improve `canonicalPath` error messages. Most important - now it respects errors from `System.Directory.canonicalizePath`
* add tests
* add travis support
* update documentation

0.2.3.0:
* add `cpathToText` that converts `CanonicalPath` to `Text`
* update base version constraints

0.2.2.0:
* add functions that return unit instead of `CanonicalPath` to `Directory` module

0.2.1.0:
* add `CanonicalPath` constructors that works with `Text` instead of `UnsafePath`

0.2.0.0:
* most of functions are in `MonadIO m` now instead of `IO`

0.1.2.0:
* add writeFile'
* add some type conversion functions
* update documentation

0.1.1.0:
* Filesystem.CanonicalPath.Directory implementation
* Add documentation

0.1.0.0:
* Initial implementation of library

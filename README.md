# CanonicalPath

**VERY IMPORTANT**

Some time ago `system-filepath` was deprecated. I haven't updated this library since, because I still think about the reasons for it to exist. Actually I have some plans on rewriting it, but it will very breaking in terms of compatibility. Keep in touch.

---

![](https://img.shields.io/travis/d12frosted/CanonicalPath.svg)
![](https://img.shields.io/hackage/v/system-canonicalpath.svg)

`FilePath` is very deceptive, because it's just a synonym for `String`, so actually it can be anything - your mothers name or path to file you want to edit. Just look at the type signature of function `readFile`:

```haskell
readFile :: FilePath -> IO String
```

You can translate it as follows:

```haskell
readFile :: String -> IO String
```

Well, it is known that IO actions are dangerous by themselves. And here comes another problem - you need to be sure that the path you pass to function is at least well constructed. For this purpose you can use well known `FilePath` data type. It solves a lot of problems and comes beefed with multiple cool utilities. And also it is build around `Text` instead of `String`. Awesome!

So why do we need yet another path library? The answer is simple - we want to use paths like `$HOME/.app.cfg`, `~/.zshrc` or `/full/path/to/existing/file/or/dir` in our code without any additional overhead. `CanonicalPath` is named so because it tries to canonicalize given path (`FilePath` or `Text`) using `canonicalizePath` function. It also will extract any variables it finds in path (like `$VARNAME`, `%VARNAME%` and special `~/`). But these steps both may fail. Thats why this library provides functions that return `Maybe CanonicalPath` or `Either Text CanonicalPath`.

`CanonicalPath` also comes with additional useful property. When it is created, it points to real file or directory. Honestly, it can't guarantee that this path will refer to existing file or directory always (someone can remove or move it to another path - and it's almost impossible to be aware of such cruelty), but you can always reconstruct `CanonicalPath`.

One more thing about path canonicalization. As I mentioned before, under the hood it uses `canonicalizePath` function. So here are two warnings. Firstly, it behaves differently on different platforms. Sometime too damn differently. So you better watch your steps. Secondly, it's impossible to guarantee that the implication `same file/dir <=> same canonicalizedPath` holds in either direction: this function can make only a best-effort attempt.

Happy Haskell Hacking!

## Documentation

All documentation can be found on [Hackage](https://hackage.haskell.org/package/system-canonicalpath).

## Contribute!

Pull requests are welcome.

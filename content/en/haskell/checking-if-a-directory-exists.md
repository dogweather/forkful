---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:56:24.447373-07:00
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
To check if a directory exists means to confirm whether a folder is actually where you think it is in the filesystem. Programmers do it to avoid errors like trying to read from a non-existent directory or inadvertently creating duplicate folders.

## How to:
Haskell uses the `directory` package for filesystem interactions. Install it with `cabal install directory` command if you haven't already. Here’s how you check for a directory:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "path/to/your/directory"
    exists <- doesDirectoryExist dir
    putStrLn $ "Does the directory exist? " ++ show exists
```

If `dir` exists, your output will be:

```
Does the directory exist? True
```

Otherwise, it’ll show:

```
Does the directory exist? False
```

## Deep Dive
Back in the day, you might've dealt directly with system calls or used libraries less abstracted than `directory`. Now, this Haskell package does the heavy lifting. 

Alternatives? You could use lower-level operations from the `unix` package, invoke shell commands, or write your own FFI bindings. All overkill for such a basic check.

Under the hood, `doesDirectoryExist` uses system-specific calls to verify the directory's presence without throwing an exception. It's an IO action, hence the need for the `main` function and `IO ()`.

## See Also
Other resources to consider:

- Your local Haskell docs: `file:///usr/share/doc/ghc/html/libraries/directory/System-Directory.html`
- Hackage for `directory` package: [https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)

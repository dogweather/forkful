---
title:                "Checking if a directory exists"
html_title:           "Haskell recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error in your code because a specific directory did not exist? Checking if a directory exists is a helpful step in ensuring your code runs smoothly, and it can save you from unexpected errors.

## How To

To check if a directory exists in Haskell, we can use the `doesDirectoryExist` function from the `System.Directory` module. Here's an example of how we can use it:

```Haskell
import System.Directory

checkDir :: FilePath -> IO Bool
checkDir dir = doesDirectoryExist dir
```

The `doesDirectoryExist` function takes in a `FilePath` as its argument and returns an `IO Bool`. The `FilePath` represents the path to the directory we want to check. Let's test it out with a few different directories:

```Haskell
checkDir "existing-directory"
-- Output: True

checkDir "non-existing-directory"
-- Output: False
```

As we can see, the function correctly returns `True` if the directory exists and `False` if it doesn't. 

## Deep Dive

Under the hood, the `doesDirectoryExist` function uses the `Dose.Directory.Internal.doesDirectoryExist` function from the `directory` package. This function makes use of system-specific calls to determine if a directory exists. For example, on Windows, it uses the Windows API while on Unix systems, it uses the `stat()` system call.

It's also worth noting that the `doesDirectoryExist` function only checks for the existence of directories and not files. We would need to use the `doesFileExist` function for that.

## See Also

- `System.Directory.doesFileExist` function: to check for the existence of a file
- `System.Directory.createDirectory` function: to create a new directory
- `System.Directory.removeDirectory` function: to remove an empty directory
- `System.Directory.removeDirectoryRecursive` function: to remove a directory and its contents
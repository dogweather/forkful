---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Haskell How-To: Check If a Directory Exists

## What & Why?

Checking if a directory exists in Haskell is simply querying the file system to determine whether a specified directory is present. This is important in programming scenarios where a read/write/update operation is to be performed, and can prevent errors caused by the directory not being present.

## How to:

Check if a directory exists with the `doesDirectoryExist` function in the `System.Directory` module.

```Haskell
import System.Directory

main = do
  doesItExist <- doesDirectoryExist "/path/to/directory"
  print doesItExist
```

Running the above program will return either `True` if the directory exists, or `False` if it doesn't.

## Deep Dive

* Historical Context: The `System.Directory` module, where the `doesDirectoryExist` function resides, has been a part of the Glasgow Haskell Compiler (GHC) libraries since its early versions. It provides programmers with an easy-to-use interface for interacting with the file system.

* Alternatives: Other than `doesDirectoryExist`, there are also functions for checking if a file exists (`doesFileExist`), or checking the type of an existing path (`pathIsSymbolicLink`, `pathIsRegularFile`, etc.). You can use them based on your specific needs.

* Implementation Details: `doesDirectoryExist` uses system-specific calls deep in the GHC's internals to interact with the file system directly. The exact implementation may vary depending on the operating system.

## See Also

Study related sources to further understand the topic.

1. GHC System.Directory Module: [hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html)
2. Real World Haskell, interacting with the file system: [book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html](https://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)
3. Haskell Wiki, Input and Output: [wiki.haskell.org/IO_inside](https://wiki.haskell.org/IO_inside)
4. Learn You a Haskell, Input and Output: [learnyouahaskell.com/input-and-output](http://learnyouahaskell.com/input-and-output)
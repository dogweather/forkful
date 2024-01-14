---
title:                "Haskell recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming, especially in Haskell. Temporary files are used to store information temporarily and are automatically deleted when the program terminates. This can be useful for tasks such as caching data, sharing files between processes, or simply to avoid cluttering the main file system.

## How To

Creating a temporary file in Haskell is a simple process. First, we need to import the `System.IO.Temp` module, which provides functions for creating and managing temporary files. Then, we can use the `withSystemTempFile` function to create the file and automatically manage its lifecycle.

```Haskell
import System.IO.Temp

main = do
  -- create a temporary file with the prefix "temp-"
  withSystemTempFile "temp-" $ \tempFilePath tempHandle -> do
    -- do something with the file handle
    hPutStrLn tempHandle "Hello, World!"
    -- the file will be automatically deleted after this block
```

In the above example, we used the `withSystemTempFile` function as a higher-order function, providing it with a function that takes two parameters: the temporary file path and the file handle. Within this function, we can manipulate the file handle as we would with any other file.

We can also specify a specific location for the temporary file by using the `withTempFile` function and providing a directory path. This can be useful when we want to ensure that the temporary file is created within a specific directory.

```Haskell
main = do
  -- create a temporary file within the "/temp" directory
  withTempFile "/temp" "temp-file.txt" $ \tempFilePath tempHandle -> do
    -- do something with the file handle
    hPutStrLn tempHandle "This is a temporary file created in the /temp directory."
    -- the file will be automatically deleted after this block
```

## Deep Dive

Behind the scenes, the `System.IO.Temp` module uses the operating system's utility functions to create temporary files. This means that the exact implementation of creating temporary files may differ based on the operating system.

In addition to `withSystemTempFile` and `withTempFile`, there are other functions in the `System.IO.Temp` module that allow more customization, such as specifying the file's permissions, file name, and directory. These functions can be useful when working with specific file systems or when we need more control over the temporary file creation process.

## See Also

- [Haskell documentation for creating temporary files](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Using Temporary Files in Haskell](https://www.fpcomplete.com/blog/2016/12/using-temporary-files-in-haskell/) by FP Complete
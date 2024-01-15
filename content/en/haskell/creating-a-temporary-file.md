---
title:                "Creating a temporary file"
html_title:           "Haskell recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files can be a useful tool for manipulating data or performing calculations without permanently altering the original source files. It allows for temporary storage and retrieval of data in a program, making it a valuable technique in various programming tasks.

## How To

Creating a temporary file in Haskell is a simple process using the `System.IO.Temp` module. To begin, import the module at the top of your file:

```Haskell
import System.IO.Temp
```

Next, use the `withSystemTempFile` function to create a temporary file and perform operations on it. The function takes in two arguments: a string that will be used as a prefix for the temporary file name, and a function that will take in a `FilePath` as a parameter.

```Haskell
withSystemTempFile "temp" $ \fp handle -> do
    hPutStrLn handle "This is a temporary file!"
    hClose handle
```

In this example, we use "temp" as the prefix and write a line of text to the file before closing it. The `withSystemTempFile` function automatically deletes the temporary file after it is closed, making it ideal for quick and temporary operations.

You can also use the `withSystemTempDirectory` function to create a temporary directory instead, following the same syntax using a prefix and a function.

## Deep Dive

Behind the scenes, the `withSystemTempFile` and `withSystemTempDirectory` functions are using the `createTempFile` and `createTempDirectory` functions respectively.

The `createTempFile` function takes in a `FilePath` and a string, which will be used as the suffix for the temporary file name. It then creates a new file with a unique name in the system's temporary directory.

Similarly, the `createTempDirectory` function creates a new temporary directory in the system's temporary directory and returns the path to it as a `FilePath`.

Both `createTempFile` and `createTempDirectory` also take in an optional third parameter for the directory to place the temporary file or directory in, by default it uses the system's temporary directory.

## See Also

- [System.IO.Temp documentation]("https://hackage.haskell.org/package/temporary-1.2.1.3/docs/System-IO-Temp.html")
- [Haskell I/O tutorial]("https://wiki.haskell.org/Introduction_to_IO")
- [Creating temporary files in other languages]("https://en.wikipedia.org/wiki/Temporary_folder#Creating_temporary_files")
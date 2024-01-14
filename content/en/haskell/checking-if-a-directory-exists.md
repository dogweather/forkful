---
title:                "Haskell recipe: Checking if a directory exists"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to check if a directory exists in your Haskell program? Maybe you want to make sure a specific file path exists before reading or writing to it. Or perhaps you want to create a new directory if it doesn't already exist. Whatever the reason may be, it's a common task in many programming projects.

## How To

Checking if a directory exists in Haskell is a simple process. First, we need to import the `System.Directory` module in our code.

```Haskell
import System.Directory
```

Next, we can use the `doesDirectoryExist` function to check if a directory exists. This function takes in a `FilePath` as an argument and returns a boolean value indicating whether the directory exists or not.

```Haskell
do dirExists <- doesDirectoryExist "path/to/directory"
   if dirExists
      then putStrLn "Directory exists"
      else putStrLn "Directory does not exist"
```

We can also use this function to create a new directory if it doesn't exist. For example, if we want to create a new directory called "images" inside an existing directory called "assets", we can use the `createDirectoryIfMissing` function.

```Haskell
createDirectoryIfMissing True "assets/images"
```

This function takes in a boolean value as the first argument, which determines whether to create intermediate directories or not. If set to `True`, it will create any intermediate directories that do not exist. 

## Deep Dive

Behind the scenes, the `doesDirectoryExist` function uses the `getDirectoryContents` and `doesFileExist` functions to check if a directory exists. It first retrieves a list of all the contents in the specified directory and then checks if any of those items are files rather than directories. If no files are found, then the function returns `False`.

The `createDirectoryIfMissing` function also uses the `doesDirectoryExist` function to check if the directory already exists. If it does, then the function does nothing. Otherwise, it uses the `createDirectory` function to create the new directory.

## See Also

- [System.Directory documentation on Hackage](https://hackage.haskell.org/package/directory)
- [Haskell File and Directory manipulation tutorial](https://www.tutorialspoint.com/haskell/haskell_file_io.htm)
- [Haskell I/O functions tutorials](https://mmhaskell.com/blog/2017/8/28/polymorphic-io-functions-in-haskell)
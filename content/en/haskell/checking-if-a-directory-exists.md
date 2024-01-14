---
title:    "Haskell recipe: Checking if a directory exists"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why Check If a Directory Exists?

As programmers, we often come across situations where we need to interact with files and directories. When working with directories, it is important to first check if they exist before performing any operations on them. This is to ensure our programs run smoothly and do not encounter errors.

## How To Check If a Directory Exists in Haskell

In Haskell, we can use the `doesDirectoryExist` function from the `System.Directory` module to check if a directory exists. This function takes in a `FilePath` as its argument and returns a `Bool` indicating whether the directory exists or not.

Let's take a look at an example:

```Haskell
import System.Directory

main = do
  let directoryPath = "myDirectory"

  exists <- doesDirectoryExist directoryPath

  if exists
    then putStrLn "The directory exists!"
    else putStrLn "The directory does not exist."
```
Output:
```
The directory exists!
```

Here, we first import the `System.Directory` module. Then, we define a `FilePath` variable for the directory we want to check. Inside the `main` function, we use the `doesDirectoryExist` function to check if the directory exists. Based on the result, we print out a corresponding message.

## Deep Dive into Checking If a Directory Exists

Let's take a deeper look into the `doesDirectoryExist` function. Underneath the hood, this function uses the `stat()` system call to check the status of a file. It checks if the `S_IFDIR` flag is set in the file's `st_mode`. This indicates that the file is a directory.

It is also worth noting that the `doesDirectoryExist` function will also return `True` if the provided `FilePath` points to a symlink that resolves to a directory.

## See Also

- [Haskell documentation for `doesDirectoryExist`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [More file and directory manipulation functions in Haskell](https://www.tutorialspoint.com/haskell/haskell_files_io.htm)
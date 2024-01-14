---
title:                "Haskell recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

If you've ever worked with file and directory manipulation in Haskell, you may have come across the need to check if a directory exists. This is an important step in ensuring your program runs smoothly and efficiently, as it allows you to handle any errors or exceptions that may arise if the directory does not exist.

## How To

To check if a directory exists in Haskell, we can use the `doesDirectoryExist` function from the `System.Directory` module. This function takes in a `FilePath` argument and returns a `Bool` value indicating whether the directory exists or not.

Let's take a look at a simple example. Say we have a directory named "haskell-blog" in the current working directory. We can check if this directory exists by using the `doesDirectoryExist` function as follows:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  exists <- doesDirectoryExist "haskell-blog"
  if exists
    then putStrLn "The directory exists!"
    else putStrLn "The directory does not exist."
```

Running this program will output "The directory exists!" since our directory does indeed exist. Now let's see what happens if we try to check for a directory that doesn't exist:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  exists <- doesDirectoryExist "non-existent-directory"
  if exists
    then putStrLn "The directory exists!"
    else putStrLn "The directory does not exist."
```

Running this program will output "The directory does not exist." since we are checking for a non-existent directory.

## Deep Dive

Behind the scenes, the `doesDirectoryExist` function uses the underlying `stat` system call to check if the given directory exists. This is a low-level system call that is used to get information about files and directories, including their existence.

It's important to note that `doesDirectoryExist` only checks for the existence of a directory, not if it is a valid directory or if you have permissions to access it. For that, you would need to use other functions such as `doesFileExist` and `getPermissions`.

## See Also

- [Haskell documentation for `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell wikibook on File IO](https://en.wikibooks.org/wiki/Haskell/File_IO)
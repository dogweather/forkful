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

## What & Why?

Checking if a directory exists is essentially a way for programmers to verify whether a certain directory or folder exists or not on their system. This can be useful in many scenarios, such as when a program needs to access files or folders stored in a specific directory, or when the program needs to create a new directory if it does not already exist.

## How to:

To check if a directory exists in Haskell, we can use the `doesDirectoryExist` function from the `System.Directory` module. This function takes in a path to the directory as an argument and returns a boolean value indicating whether the directory exists or not.

```Haskell
import System.Directory

main = do
  let path = "./mydirectory" -- replace with the path to your desired directory
  exists <- doesDirectoryExist path
  if exists
    then putStrLn "Directory exists"
    else putStrLn "Directory does not exist"
```

Sample output when the directory exists:

```
> Directory exists
```

Sample output when the directory does not exist:

```
> Directory does not exist
```

## Deep Dive:

### Historical Context:

The `System.Directory` module has been a part of the Haskell standard library since version 1.2.5. The `doesDirectoryExist` function was introduced in the 1.4.1 release, making it a relatively recent addition to the standard library.

### Alternatives:

Besides using the `doesDirectoryExist` function, there are a few other ways to check if a directory exists in Haskell. One such alternative is the `findFile` function from the `Filesystem` module. This function searches for a file or directory with the given name in the specified directory, and if found, it returns the full path to the file or directory.

### Implementation Details:

The `System.Directory` module uses the `stat` system call to determine if a directory exists or not. It returns an `EitherIOError Bool` value, where `Left` represents an error and `Right` represents the boolean value indicating whether the directory exists.

## See Also:

- [Haskell System.Directory module documentation](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Filesystem module documentation](https://hackage.haskell.org/package/Filesystem/docs/Filesystem.html)
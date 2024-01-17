---
title:                "Checking if a directory exists"
html_title:           "Elm recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a common task for programmers, especially when working with file systems. It is essentially a way to verify if a specific directory (or folder) exists in a given location. Programmers do this to ensure that their code properly handles the situation when a directory is missing, and to avoid errors or unexpected behavior.

## How to:

To check if a directory exists in Elm, you can use the built-in `Directory` module. It provides a `exists` function that takes in a `String` representing the directory path, and returns a `Task` with a `Bool` value indicating whether the directory exists or not.

```
Elm.Task
    .attempt Directory.exists "/path/to/directory"
    .map (\result -> case result of
        Err _ -> -- handle error
        Ok exists -> -- use exists value
    )
```

If the directory exists, the `exists` value will be `True`, otherwise it will be `False`.

## Deep Dive

Historically, checking if a directory exists was a crucial step in file management, especially in operating systems where file permissions and ownership were stricter. It was also important for error handling to prevent invalid paths from being accessed.

An alternative to using the `exists` function is to use `FileSystem.access` which checks for both file and directory existence. However, this approach requires different handling for each type of existence, making the code less concise.

The implementation of `exists` in the `Directory` module utilizes the `stat` system call to retrieve information about the directory. It then checks for specific flags in the returned data to determine if the directory exists.

## See Also

- [Elm Directory Module](https://package.elm-lang.org/packages/elm/core/latest/Directory)
- [Elm FileSystem Module](https://package.elm-lang.org/packages/elm/filesystem/latest/FileSystem)
- [stat system call](https://linux.die.net/man/2/stat)
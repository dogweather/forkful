---
title:    "Elm recipe: Checking if a directory exists"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to check if a directory exists in your Elm program? This could be useful in scenarios where you need to create a directory if it doesn't exist or perform a certain action depending on the existence of a directory.

## How To 

Checking if a directory exists in Elm is a simple task. First, we need to import the `Directory` module from `elm/file`, since file operations in Elm are handled by this module. Then, we can use the `doesDirectoryExist` function to check if a directory exists.

```Elm
import File exposing (Directory, doesDirectoryExist)

-- check if a directory named "data" exists
doesDirectoryExist (Directory "data")
    --> False
```

In the above example, we are checking if a directory named "data" exists and the output is `False` since we haven't created this directory yet. We can also use the `doesDirectoryExist` function to check if a given path is a directory.

```Elm
import File exposing (Directory, doesDirectoryExist)

-- check if path "elm/data" is a directory
doesDirectoryExist (Directory "elm/data")
    --> True
```

If the given path is not a directory, the function will return `False`.

## Deep Dive 

Under the hood, the `doesDirectoryExist` function uses the `getFileStatus` function from the `elm/file/FileInfo` module. This function returns information about a given file or directory, including its `type`. If the `type` is `Directory`, then the directory exists. 

## See Also 

- [Elm File Documentation](https://package.elm-lang.org/packages/elm/file/latest/) 
- [Elm FileInfo Documentation](https://package.elm-lang.org/packages/elm/file/latest/FileInfo)
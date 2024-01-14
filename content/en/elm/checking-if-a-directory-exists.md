---
title:    "Elm recipe: Checking if a directory exists"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Why Check If a Directory Exists in Elm
Sometimes in programming, we need to check if a certain directory exists before performing any operations on it. This can be helpful in avoiding errors or unexpected behavior in our code. In this blog post, we will explore how we can do this in Elm.

## How To Check If a Directory Exists
To check if a directory exists in Elm, we can use the `filesystem` package. If you haven't already, you can install this package by running the following command in your terminal:
```Elm
elm install elm/filesystem
```

Once the package is installed, we can import it in our code using the following line:
```Elm
import Filesystem exposing (..)
```

Next, we need to specify the path of the directory we want to check. We can do this by using the `fromString` function and passing in the path as a string. For example, if we want to check if the directory "my_directory" exists in the current directory, our code would look like this:
```Elm
let
    directoryPath = fromString "my_directory"
in
    ...
```

Now, we can use the `directoryExists` function to check if the directory exists. This function takes in the directory path and returns a `Task` with a `Bool` value. We can then use `andThen` to perform some action based on the result of the `Task`. For example, if we want to log a message when the directory exists, our code would look like this:
```Elm
let
    directoryPath = fromString "my_directory"
in
    andThen (\exists -> if exists then log "Directory exists!" else log "Directory does not exist") (directoryExists directoryPath)
```

## Deep Dive into Checking for Directory Existence
The `Filesystem.directoryExists` function in the `filesystem` package actually uses the `stat` function from the native JavaScript API to determine the existence of the directory. If the `stat` function returns an error, it means the directory does not exist.

It's also important to note that this method only checks for the existence of the directory, not the accessibility permissions. So even if the directory exists but the user does not have permission to access it, the `directoryExists` function will still return `False`.

## See Also
- [Filesystem package documentation](https://package.elm-lang.org/packages/elm/filesystem/latest/)
- [Elm directoryExists function source code](https://github.com/elm/filesystem/blob/master/src/Filesystem.elm#L84)
- [JavaScript stat function documentation](https://nodejs.org/api/fs.html#fs_statsync_path_options)
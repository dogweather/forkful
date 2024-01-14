---
title:                "Elm recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to check if a directory exists in your Elm program? Maybe you want to handle different scenarios depending on whether a directory is present or not. Or perhaps you want to make sure a specific directory is created before performing certain actions. Whatever the reason may be, knowing how to check if a directory exists in Elm can come in handy in various situations.

## How To

To check if a directory exists in Elm, we can use the FileSystem module from the elm/file package. First, we need to import the module at the top of our Elm file.

```Elm
import FileSystem exposing (exists)
```

Next, we can call the `exists` function, passing in the path of the directory we want to check. This function will return a `Task Bool`, which represents an asynchronous operation that will eventually produce a Boolean value.

```Elm
checkDirectoryExists : Task Bool
checkDirectoryExists =
  exists "path/to/directory"
```

We can then use `Task.perform` to handle the outcome of the `exists` task. In case of success, the `perform` function will execute the `Result.withDefault` function, which will return a default value of `False` if the task fails or the Boolean value if it succeeds.

```Elm
directoryExists : Bool
directoryExists =
  Task.perform (Result.withDefault False) checkDirectoryExists
```

If the directory exists, the `directoryExists` value will be `True`. Otherwise, it will be `False`.

## Deep Dive

Internally, the `exists` function uses the `Stat` module from the elm/file package to get information about the given path and check if it is a directory or not. If the path does not exist, the task will fail, and we will receive a `False` value. Otherwise, the task will succeed, and we will receive a `True` value.

## See Also

Here are some useful resources for further reading on checking if a directory exists in Elm:

- [Elm file package documentation](https://package.elm-lang.org/packages/elm/file/latest/)
- [FileSystem module source code](https://github.com/elm/file/blob/latest/src/FileSystem.elm)
- [Stat module source code](https://github.com/elm/file/blob/latest/src/Stat.elm)
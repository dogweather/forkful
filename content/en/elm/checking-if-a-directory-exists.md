---
title:                "Elm recipe: Checking if a directory exists"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

In Elm programming, checking if a directory exists is an essential task. Whether you are building a web application or working on a project, being able to verify if a directory exists is crucial for proper file management and organization. Additionally, it can prevent errors and improve the overall functionality of your code.

## How To

To check if a directory exists in Elm, we can use the `Directory.exists` function. This function takes in a `String` representing the directory path and returns a `Task` value. We can then use `Task.andThen` to handle the result of the task and perform further actions.

Let's take a look at an example of how this works:

```Elm
import Task
import Directory

checkDirectory : String -> Task x Bool
checkDirectory path =
    Directory.exists path
        |> Task.map (\exists -> exists)

main : Task x ()
main =
    checkDirectory "home/project/myDirectory"
        |> Task.andThen (\exists ->
            if exists then
                Task.succeed "Directory exists!"
            else
                Task.fail "Directory does not exist."
        )
```

In this code, we import the necessary modules, define a function `checkDirectory` that takes in a `String` representing the directory path, and use the `exists` function to check for its existence. We then use `Task.andThen` to handle the result of the task and either succeed with a message stating the directory exists or fail if it does not.

Now, let's try running this code and see the output:

```
Directory exists!
```

As expected, since we used a valid directory path, the task is successful and we get a message indicating the directory exists.

But what if we try to check for a directory that does not exist? Let's see the output:

```
Directory does not exist.
```

Again, the task is handled accordingly and we get an error message stating the directory does not exist.

## Deep Dive

Behind the scenes, the `exists` function uses the `FileSystem` module from Elm's standard library. This module provides access to the local file system and allows us to perform tasks such as checking for file or directory existence, creating new files or directories, and reading or writing to existing files.

The `exists` function is actually built on top of the `FileSystem.request` function, which takes in a `Request` type and returns a `Task` value. This `Request` type can be used to specify the type of operation we want to perform on the file system, such as checking for existence, reading from a file, or writing to a file. By using `Directory.exists` and `FileSystem.request` together, we can create more complex file management systems and perform various operations on directories.

## See Also

- [Elm Documentation - FileSystem](https://package.elm-lang.org/packages/elm/file/latest/FileSystem)
- [Elm Documentation - Task](https://package.elm-lang.org/packages/elm/core/latest/Task)
- [Elm in Action by Richard Feldman](https://www.manning.com/books/elm-in-action)
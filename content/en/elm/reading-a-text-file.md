---
title:                "Elm recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, there are endless possibilities and techniques to learn. One of the most important skills to have as a programmer is the ability to read and understand text files. In this blog post, we will explore how to read a text file using Elm, a functional programming language that is gaining popularity for its simplicity and powerful features.

## How To

To read a text file in Elm, you first need to create a function that will handle the file reading. You can do this by using the `File` library, which provides convenient functions for working with files.

```Elm
import File

readFile : String -> Task x String
readFile path =
    File.readString path
        |> Task.toResult
        |> Task.mapError toString
```
In the above code, we import the `File` library and define a function called `readFile` that takes in a string (the path to the text file) and returns a `Task`, which represents an asynchronous computation that may succeed with a value or fail with an error. The `File.readString` function reads the contents of the file and returns a `Task` that contains the text.

To actually execute the `Task`, we can use the `Task.perform` function, which takes in two functions as arguments: one for handling success and one for handling error.

```Elm
readFile "example.txt"
    |> Task.perform
        (\text -> -- handle success
            text
                |> String.lines
                |> List.indexedMap (\i line -> (i+1, line))
                |> String.join "\n"
                |> Debug.log "file contents"
        )
        (\err -> -- handle error
            Debug.log "error reading file: " ++ err
        )
```

The first function passed to `Task.perform` takes the text from the file and performs some actions with it. In this case, we use the `String.lines` function to split the text into a list of lines, then use `List.indexedMap` to add line numbers to each line, and finally, we use `String.join` to join the lines back together with a newline between them. The `Debug.log` function is used to log the result to the console.

The second function handles any errors that may occur while reading the file. In this example, we simply log the error to the console.

## Deep Dive

Reading a text file may seem like a simple task, but there are some important things to keep in mind. First, make sure that the text file you are trying to read actually exists and is accessible by your code. Otherwise, errors may occur.

Second, it is important to handle any errors that may occur while reading the file. This could be due to an incorrect file path, a corrupt file, or other reasons. By using the `Task` type, we can easily handle success and failure scenarios in our code and make sure our program does not crash unexpectedly.

Lastly, remember to always close the file after reading it to avoid any potential memory leaks.

## See Also

- [Elm File library documentation](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Elm official website](https://elm-lang.org/)
- [Elm tutorials and resources](https://github.com/isRuslan/awesome-elm#resources)
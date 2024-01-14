---
title:                "Elm recipe: Reading a text file"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Are you tired of manually typing in data into your Elm program? Do you want to easily import data from a text file instead? Well, you're in luck! In this blog post, we will explore how to read a text file in Elm, making your coding process more efficient and streamlined.

## How To

First, we need to import the `Text` module in order to use the `Text.fromFile` function. This function takes in a string representing the file path and returns a `Task` with either an error message or the contents of the file. Let's see an example:

```Elm
import Text exposing (fromFile)

readFile : Task String String
readFile =
  fromFile "myFile.txt"

```

In the above code, we are importing the `fromFile` function and then using it to read the contents of a file called "myFile.txt". Notice that the function returns a `Task` with a `String` representing any error message and a `String` with the actual contents of the file. We can then use the `Task.perform` function to handle the result.

```Elm
import Text exposing (fromFile)

readFile : Task String String
readFile =
  fromFile "myFile.txt"

handleFile : Result String String -> String
handleFile result =
  case result of
    Ok contents ->
      contents
    Err error ->
      error

readFileTask : Task String String
readFileTask =
  Task.perform handleFile readFile
```

In the above code, we are using the `Task.perform` function to handle the result of the `readFile` function. If the result is an `Ok`, we simply return the contents of the file. If the result is an `Err`, we return the error message. Now, we can use the `readFileTask` to read the file and print its contents:

```Elm
readFileTask |> Task.attempt
-- outputs: "This is the content of the file."
```

## Deep Dive

Let's take a deeper look at the `fromFile` function. Internally, it uses the `Text.readFile` function which reads a file asynchronously and returns a `Text.File` with the contents. The `fromFile` function then takes this result and wraps it in a `Task` for easier handling. This is why we use the `Task.perform` function to handle the result.

It's worth noting that the `fromFile` function expects the file to be encoded in UTF-8. So if your file is using a different encoding, you may need to use the `Text.readFileWith` function and specify the correct encoding.

## See Also

- [Official Text module documentation](https://package.elm-lang.org/packages/elm/core/latest/Text)
- [Elm guide on handling tasks](https://guide.elm-lang.org/error_handling/tasks.html)
- [Elm syntax highlighting for Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=sbrink.elm)
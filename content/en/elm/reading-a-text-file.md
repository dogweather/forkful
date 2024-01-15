---
title:                "Reading a text file"
html_title:           "Elm recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're an English reader looking to learn Elm, chances are you're either a beginner programmer or you're interested in functional programming. Whatever the case may be, learning how to read a text file in Elm is a useful skill to have. It allows you to easily access and manipulate data from external sources, such as APIs or user-generated files.

## How To

Reading a text file in Elm is fairly straightforward. First, we need to import the necessary module by adding `import File` to the top of our code. Next, we can use the `File.read` function to read in a file. Here's an example:

```Elm
import File

readTextFile : (Result String String -> msg) -> Sub msg
readTextFile callback =
  File.read "example.txt" callback
```
In this example, we're using a callback function to handle the result of the file read operation. The `File.read` function takes in the file path and the callback function as its arguments. Once the file has been read, the callback function will be called with a `Result` type, which can either be `Ok` or `Err`.

To access the actual contents of the file, we can use the `Result.withDefault` function. Here's an example:

```Elm
import File exposing (read)
import Result exposing (withDefault)

main =
  read "example.txt" (`withDefault` "") -- default value to return if file cannot be read
```

In this example, we're using the `withDefault` function to handle potential errors when reading the file. If the file cannot be read, the default value of an empty string will be returned.

## Deep Dive

Now, let's take a deeper look at the `File.read` function and its type signature:

```
read : String -> (Result String String -> msg) -> Sub msg
```

As we can see, the function takes in a `String` (representing the file path) and a callback function with the `Result String String` type. This type represents a `Result` that either contains a `String` or an error message.

Additionally, the `File.read` function returns a `Sub` type, which is a type used in Elm for handling subscriptions to external events. This is because reading a file is an asynchronous operation, meaning that we want to wait for the file to be read before continuing with our program.

## See Also

- [Elm Docs - File](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm Tutorial - Reading and Writing Files](https://elmprogramming.com/reading-files-elm.html)
- [Codepen - Reading Text File in Elm](https://codepen.io/sharkdp/pen/eYJWeQJ)
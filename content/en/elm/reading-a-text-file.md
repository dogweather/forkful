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

## What & Why?

Reading a text file is the process of extracting data from a file that is in a plain text format. Programmers often do this to access important information or to manipulate and analyze data. It is a vital skill for any programmer, especially for those working with large datasets or creating applications that need to process text-based information.

## How to:

To read a text file in Elm, we can use the `Text` library's `fromString` function. This function takes in a string containing the file's contents and converts it into a `Text` value, which can then be used for further processing. 

Example:

```Elm
import Text

readFile : String -> Task err Text
readFile path =
  File.read path
    |> Task.map Text.fromString
```

The above code uses the `File.read` function from the `File` library, which reads the contents of a file into a `String`. This `String` is then converted into a `Task` using the `Task.map` function, which allows us to perform some action on the value once it is available.

To use this function, we need to provide the path of the text file we want to read. This can be done by passing the path as an argument to the `readFile` function.

Output:

```
Task { value = Ok "This is the contents of the text file." }
```

As shown in the output, the file's contents are wrapped in the `Task` type, which allows for asynchronous handling of data. To extract the actual value, we can use the `Task.attempt` function, which takes in a `Decoder` and returns a `Task` with the decoded value. 

Example:

```Elm
import Task exposing (attempt)
import Json.Decode exposing (Decoder, string)

fileDecoder : Decoder String
fileDecoder =
  string

getFileContents : String -> Task String String
getFileContents path =
  readFile path
    |> attempt fileDecoder
```

We can then use the `getFileContents` function to get the contents of the file and use them in our application.

## Deep Dive:

The ability to read text files has been a part of programming since the early days, with the first programming languages like COBOL supporting file I/O operations. In Elm, reading text files was previously possible using the `Http` library, but it was removed in version 0.19 due to security concerns.

An alternative to using the `Task` type to handle file reading is to use the `Browser.File` library, which allows for selecting a file from the user's system and reading it directly. However, this approach is only suitable for applications running in the browser and not native applications.

Implementation details of reading text files in Elm can be found in the `File` and `Text` libraries, which use native code to access the system's file system and convert the file's contents into a usable format.

## See Also:

- Elm File: https://package.elm-lang.org/packages/elm/file/latest/
- Elm Text: https://package.elm-lang.org/packages/elm/core/latest/Text
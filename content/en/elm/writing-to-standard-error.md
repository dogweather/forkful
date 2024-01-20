---
title:                "Writing to standard error"
html_title:           "Elm recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Elm allows programmers to display error messages or debug information to a designated console instead of the regular output. This helps developers to easily identify and troubleshoot issues while their code is running.

## How to:

To write to standard error in Elm, you can use the `Debug.crash` function. Here's an example:

```Elm
import Debug exposing (crash)

sayHello : String -> String
sayHello name =
    if String.isEmpty name then
        crash "Name cannot be empty!"
    else
        "Hello, " ++ name ++ "!"

sayHello ""
```

The above code will write the error message "Name cannot be empty!" to the console instead of producing a regular output.

## Deep Dive:

In the early days of programming, developers used to print error messages and debug information directly to the output stream, which made it difficult to distinguish them from the regular output. Writing to standard error was introduced as a way to separate these messages and make debugging easier.

In Elm, the `Debug.crash` function is the only way to write to standard error. Other programming languages may have multiple options for writing to this stream, such as `System.err` in Java or `Console.error` in JavaScript.

## See Also:

- [Debug module documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
---
title:                "Writing a text file"
html_title:           "Elm recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file means creating or modifying a file with textual information. Programmers often write text files as a convenient way to store and organize data in a human-readable format. This allows for easier maintenance and sharing of data across different programming languages and systems.

## How To:

To write a text file in Elm, we can use the `Text` library. The `Text` library provides functions to create and modify text files.

```Elm
import Text exposing (writeFile, concat)

writeFile "newfile.txt" (concat ["Hello,", " world!"])
```

Running this code will create a new file called `newfile.txt` in the same directory as our Elm file, with the text "Hello, world!" written in it.

We can also append text to an existing file using the `appendFile` function.

```Elm
import Text exposing (appendFile)

appendFile "existingfile.txt" "This is an example sentence."
```

## Deep Dive:

Text files have been around for a long time, and they are one of the simplest and most common ways to store data. They are also versatile, as they can be opened and modified with different programs and programming languages.

There are other ways to store data, such as using databases or binary files, but text files remain a popular choice due to their simplicity and compatibility.

When writing a text file in Elm, we can also provide options such as encoding and file permissions as needed. For more advanced text file operations, we can use the `File` module in Elm.

## See Also:

- Elm's official documentation for the `Text` and `File` modules: https://package.elm-lang.org/packages/elm/core/latest/
- A tutorial on working with text files in Elm: https://www.elm-tutorial.org/en/05-foundations/03-strings.html
---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings involves retrieving specific subsets of string data based on user-defined parameters, such as start and end index. This common operation allows programmers to manipulate and utilize pieces of larger strings effectively.

## How To:

In Elm, the `String.slice` function does the job by taking the start and end indices as arguments. It returns the substring from the start index up to (but not including) the end index. If the end index exceeds the string length, it'll use the string length.

Here's a simple example:

```Elm
import Html exposing (text)
import String

main =
  text (String.slice 0 3 "Hello, world!")
```
In this code, we're grabbing the first three characters of the string "Hello, world!". So the output will be "Hel".

## Deep Dive

`String.slice` has roots back in JavaScript, and its function is identical in Elm. Extracting substrings can be done in various ways based on the programming language you are using. JavaScript offers `substring` and `substr`, Python has `slice` and `substring` operations, and in Elm, it's `String.slice`.

Remember, the `String.slice` function can take negative numbers as indices. Negative indices count from the end of the string, -1 being the last character. However, Elm doesn't support the usage of negative start and end indexes. 

## See Also

String Manipulation in Elm: [https://guide.elm-lang.org/interop/](https://guide.elm-lang.org/interop/)

Elm String Library Documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String) 

Happy Coding!
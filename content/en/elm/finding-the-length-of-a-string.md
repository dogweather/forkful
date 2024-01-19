---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string involves counting the number of characters in a string. Programmers do this to control text display, limit input, and manage data inside of string variables.

## How to:

Here's how you find the length of a string in Elm:

```Elm
import String

main =
  String.length "Hello, World!"
```

After you run this program, it will return `13`, which is the length of the string "Hello, World!".

## Deep Dive

The `String.length` operation in Elm, despite its simplicity, has a meaningful history and offers alternatives.

1. **Historical context**: Elm is unique because it's a frontend language that carries immutable data and pure functions, with likely comparison being Haskell. The concept of string length is common in any language but how it is implemented in Elm reveals the functional programming beauty. 

2. **Alternatives**: In Elm, you won’t find many alternatives to `String.length` due to the principle of simplicity the language follows. However, writing custom function is theoretically possible, though likely unneeded due to the simple and efficient solution already available.

3. **Implementation**: Elm’s `String.length` function is an O(1) operation, meaning it’s incredibly efficient. When working with larger, more complicated programs, `String.length` won't hold you back in terms of performance.

## See Also:

For further reading and examples, refer to these sources:

- [Elm's Official String Guide](https://package.elm-lang.org/packages/elm/core/latest/String#length): The definitive source on strings in Elm, including `String.length`.
- [Elm Exercises](https://elmprogramming.com/strings.html): This page provides additional practical examples involving strings.
- [Elm's Style Guide](https://elm-lang.org/docs/style-guide): If you want to write clean, idiomatic Elm code, this guide is the gold standard.
- [Elm's Syntax Guide](https://elm-lang.org/docs/syntax): A comprehensive overview of Elm's syntax and core language constructs, including string manipulations.
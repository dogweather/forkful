---
title:                "Capitalizing a string"
html_title:           "Haskell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means converting all letters in a string to uppercase. Programmers often do this for consistency and readability in their code, as well as for conforming to certain formatting standards.

## How to:

```Haskell
import Data.Char

capitalize :: String -> String
capitalize = map toUpper
```
Sample output: "hello world" -> "HELLO WORLD"

## Deep Dive:

### Historical Context:

Capitalizing strings has been a common practice in programming languages since the early days of computer programming. In many cases, uppercase letters were easier to read on early monitors and punch cards. Additionally, the use of uppercase letters helped to distinguish keywords and variables in a program.

### Alternatives:

Aside from using the `Data.Char` library, there are other ways to capitalize a string in Haskell. For example, you could use recursion to iterate through each character in the string and convert it to uppercase. Another alternative is to use the `Char` datatype's `isLower` and `toUpper` functions to only convert lowercase letters to uppercase.

### Implementation Details:

The `capitalize` function in our example makes use of the `map` function to apply the `toUpper` function to each character in the string, using function composition. This allows for a concise and readable implementation of the `capitalize` function.

## See Also:

- [toUpper function documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toUpper)
- [map function documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
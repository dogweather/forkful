---
title:                "Capitalizing a string"
html_title:           "Elm recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string simply means converting the first letter of each word in the string to uppercase. This is a common practice in programming for improving readability and consistency. It also follows certain naming conventions, making the code more standardized.

## How to:

```Elm
import String

String.capitalize "hello world"
-- Output: "Hello World"

String.capitalize "hello elm"
-- Output: "Hello Elm"
```

In the above examples, we used the `String.capitalize` function from the `String` module in Elm. This function takes in a string as an argument and returns a new string with the first letter of each word capitalized. It does not modify the original string.

## Deep Dive:

The practice of capitalizing strings has been around since the early days of computer programming. It originated from the typographical convention of using uppercase letters for emphasis or headings. In programming, it became a way to distinguish between different types of data and improve code readability.

In Elm, there are other ways to capitalize strings besides using the `String.capitalize` function. One alternative is using the `String.toUpper` function, which converts all letters in the string to uppercase. This may not produce the desired result, especially if the string contains special characters or symbols.

The `String.words` function can also be used to split a string into a list of words, which can then be converted to uppercase using `List.map` and `String.toUpper`. This approach allows for more flexibility in handling different types of strings.

## See Also:

- Elm Documentation on `String`: https://package.elm-lang.org/packages/elm-lang/core/latest/String
- Functional Programming in Elm: https://guide.elm-lang.org/architecture/
- String capitalization best practices: https://dev.to/sandrastring/string-capitalization-best-practices-565
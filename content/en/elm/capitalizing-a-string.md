---
title:                "Elm recipe: Capitalizing a string"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalize Strings in Elm

Capitalizing strings is a common task in programming, and in Elm, it can easily be done using built-in functions. In this blog post, we will explore how to capitalize strings in Elm and why it may be useful in certain scenarios.

## How To

To capitalize a string in Elm, we can use the `toUpper` function from the `String` module. This function takes in a string and returns a new string with all characters converted to uppercase.

```
Elm String.toUpper

-- Example usage
string = "hello world"
capitalized = String.toUpper string
-- Output: "HELLO WORLD"
```

We can also use the `toUpperFirst` function to capitalize only the first character of a string.

```
Elm String.toUpperFirst

-- Example usage
string = "hello world"
capitalized = String.toUpperFirst string
-- Output: "Hello world"
```

## Deep Dive

In Elm, strings are immutable, which means they cannot be changed in-place. Therefore, when we use `toUpper` or `toUpperFirst` functions, they return a new string instead of modifying the original one.

Additionally, these functions work with Unicode characters, making them suitable for internationalization. Elm also provides other string functions that can be useful in manipulating and formatting strings, such as `trim`, `split`, and `join`.

## See Also

- Official Elm Documentation for Strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Programming Language Website: https://elm-lang.org/
---
title:                "Elm recipe: Concatenating strings"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, and it allows us to combine multiple string values into one meaningful string. This can be useful in many situations, such as displaying a message or building a URL.

## How To

To concatenate strings in Elm, we can use the `++` operator or the `concat` function. Let's take a look at some examples:

```
concatenatedString = "Hello " ++ "World" -- Output: "Hello World"
```

```
URL = concat ["https://", "example.com", "search?query=", "elm"] -- Output: "https://example.com/search?query=elm"
```

In the first example, we used the `++` operator to combine the strings "Hello " and "World" into a new string "Hello World". In the second example, we used the `concat` function to join an array of strings into one string, which can be useful when building URLs or formatting messages.

## Deep Dive

In Elm, strings are represented as a list of characters. When we use the `++` operator or `concat` function, the strings are converted into a list of characters, concatenated, and then converted back into a string. This means that we can also use the `++` operator or `concat` function to concatenate characters or lists of characters.

```
greeting = 'H' ++ 'e' ++ 'l' ++ 'l' ++ 'o' -- Output: "Hello"
```

```
combinedList = concat [['H', 'e'], ['l', 'l'], ['o']] -- Output: "Hello"
```

## See Also

- Elm documentation on string concatenation: https://package.elm-lang.org/packages/elm/core/latest/String#++
- Elm strings and characters guide: https://guide.elm-lang.org/strings/
- Concatenate strings in other programming languages: https://www.geeksforgeeks.org/python-program-to-concatenate-two-strings/
---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a common operation in text processing. This helps in cleaning up data, removing unwanted parts, or prepping input for further parsing.

## How to:

Elm offers no built-in function to directly delete characters matching a pattern. Instead, you can use `String.split` and `String.join` functions to reach the same result. Let's say you want to remove all occurrences of the letter 'a' from a string:

```Elm
removeChar : Char -> String -> String
removeChar char str =
    str
        |> String.split (String.fromChar char)
        |> String.join ""

-- in usage
removeChar 'a' "banana" 
-- Returns "bnn"
```

## Deep Dive

Deleting characters matching a particular pattern is an operation going back as far as the earliest days of text processing and is ubiquitous in every high-level programming language. Elm, being a frontrunner for functional programming in the front-end world, takes a different approach, adhering closely to functional programming principles.

An advantage of using `String.split` and `String.join` in place of a built-in replace function is that it lends itself well to function composition. It's a testament to Elm's philosophy of making data transformations a series of small, understandable steps.

An interesting detail about Elm's `String.split` is that it doesn't treat the input pattern as a regular expression, but as a literal string. Elm intentionally lacks regex support in its core libraries, pushing developers toward a more declarative style of string processing.

## See Also

1. String Processing in Elm: [https://elmprogramming.com/strings.html](https://elmprogramming.com/strings.html)
2. String functions in Elm: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
3. More on Functional Programming in Elm: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
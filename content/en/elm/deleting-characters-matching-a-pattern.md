---
title:    "Elm recipe: Deleting characters matching a pattern"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Deleting characters from a string may seem like a simple task, but what if you only want to delete characters that match a specific pattern? Whether you're trying to clean up user input or parse through a large dataset, being able to delete characters matching a pattern can be a useful skill to have in your Elm programming arsenal.

## How To
To delete characters matching a pattern in Elm, we can use the `String.replace` function. This function takes in two arguments: the pattern to be replaced and the replacement string. Take a look at the following example:

```elm
import String exposing (replace)

replace "a" "" "apple" -- result: "pple"
```

In this example, we are replacing all occurrences of the letter "a" with an empty string, effectively deleting it from the original string.

We can also use regular expressions as our pattern, giving us even more control over which characters we want to delete. For example, if we want to delete all non-numerical characters from a phone number, we could use the following code:

```elm
import Regex exposing (replace)

replace (Regex.regex "[^0-9]") "" "(123) 456-7890" -- result: "1234567890"
```

The above code uses a regular expression to match all non-numerical characters (denoted by `[^0-9]`) and replaces them with an empty string, leaving us with a clean phone number.

## Deep Dive
Behind the scenes, the `String.replace` function uses the `replace` function from the `Regex` module. Regular expressions are powerful tools for matching and manipulating patterns in strings, and Elm's `Regex` module provides a variety of functions for working with them.

Some other useful functions from the `Regex` module for deleting characters matching a pattern include `replaceAll` which replaces all occurrences of a pattern with a replacement string, and `replaceFirst` which only replaces the first occurrence of the pattern.

## See Also
- `String` module documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- `Regex` module documentation: https://package.elm-lang.org/packages/elm/regex/latest/Regex
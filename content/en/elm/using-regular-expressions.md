---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in strings. Programmers use them for searching, editing, or manipulating text, simplifying tasks like form validation or parsing data.

## How to:

Elm doesn't have built-in regex capabilities, but you can use the `elm/regex` package. Here's how to use regex for common tasks:

```Elm
import Regex exposing (..)

-- Examples of regex usages in Elm --

-- Checking if a string contains "hello"
checkForHello : String -> Bool
checkForHello input =
    let
        pattern = "hello"
        regex = Regex.fromString pattern |> Maybe.withDefault (regex ".")
    in
    Regex.contains regex input

-- Sample Output
checkForHello "hello, world!" -- True

-- Extracting digits from a string
extractDigits : String -> List String
extractDigits input =
    let
        regex = Regex.fromString "\\d+" |> Maybe.withDefault (regex ".")
    in
    Regex.find (All) regex input |> List.map .match

-- Sample Output
extractDigits "elm123rocks" -- ["123"]
```
Remember, you need to handle Maybe for potential pattern-matching failures when using `Regex.fromString`.

## Deep Dive

Regex goes back to the 1950s, with roots in automata theory and formal language theory. Over time, regex became a powerful tool in text processing, integrated into many programming languages and command-line utilities.

Alternatives to regex in Elm include string functions like `String.contains`, `String.startsWith`, `String.split`, etc. While simpler, they're less powerful for complex pattern matching.

Implementation-wise, regex in Elm is built on top of JavaScript's regex engine, courtesy of Elm's runtime. This means regex behavior in Elm can mirror JavaScript's capabilities and limitations.

## See Also

- Elm Regex Package: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Regular Expressions in JavaScript: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex Testers and Debuggers: [regex101.com](https://regex101.com)

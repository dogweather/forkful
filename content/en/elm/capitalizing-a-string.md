---
title:                "Capitalizing a string"
date:                  2024-01-19
simple_title:         "Capitalizing a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means converting the first character to uppercase and keeping the rest lowercase. Programmers do this for proper nouns, titles, or to enforce a consistent style for headings and text content.

## How to:
In Elm, you don't get a capitalize function out of the box, but you can easily create one:

```Elm
import String exposing (toUpper, toLower, left, dropLeft)

capitalize : String -> String
capitalize text =
    if String.isEmpty text then
        ""
    else
        toUpper (left 1 text) ++ toLower (dropLeft 1 text)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Output: "Hello Elm World"
```

## Deep Dive
Elm prefers explicitness and does not include common string manipulations like `capitalize` in the core libraries. Historically, you'd either roll your own solution or pull in a third-party library that extends `String` manipulations. 

Elm's core `String` library provides `toUpper` and `toLower`, which handle full string transformations. To capitalize, you take the first character using `left`, uppercase it with `toUpper`, and then append it to the remainder of the string, turned lowercase by `toLower`. The remaining part of the string is extracted using `dropLeft`, which avoids affecting the first character.

While Elm's standard libraries may lack a native `capitalize` function, the decision ensures a minimalist and performant core, leaving such specific utilities to userland implementation or additional packages.

Alternatives include using full-blown string manipulation packages like `elm-string-extra`, which include a `capitalize` function, among other helpful string operations:

```Elm
import String.Extra exposing (capitalize)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Output: "Hello Elm World"
```

Note that Elm's approach to strings is Unicode-aware, which means it handles capitalization correctly even for languages with non-Latin alphabets, though with additional complexities.

## See Also
- Elm `String` documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- `elm-string-extra` library on Elm packages: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Unicode standard for case mappings: https://www.unicode.org/reports/tr21/

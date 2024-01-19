---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are sequences used to match character combinations in strings. Programmers use them for pattern matching with strings, or string searching and replacing operations.

## How to:
In Elm, the `Regex.regex` function is used to create a pattern, and `contains` checks if that pattern exists within a string. Here's an example:

```Elm
import Regex

pattern : Maybe Regex.Regex
pattern =
    Regex.regex "Elm"

main =
    let
        res =
            case pattern of
                Nothing ->
                    False

                Just re ->
                    Regex.contains re "Hello Elm!"
    in
    Html.text <| toString res
```

Run this and you'll get `True` because the string "Hello Elm!" contains "Elm".

## Deep Dive
Elm's regular expressions come from JavaScript, and hence carry some JavaScript flavor. They're defined as ‘patterns’ of characters used in pattern matching with strings. Alternatives to regex in Elm could be functions like `String.startsWith`, `String.endsWith` or `String.contains`. These are good when you're dealing with simple matching scenarios, but nothing beats regex when it comes to complex pattern searches and manipulations. Implementation wise, Elm uses JavaScript regex under the hood.

## See Also
Delve deep into regex with official Elm documentation at: http://package.elm-lang.org/packages/elm/regex/latest/Regex
Or explore JavaScript's Regex, which Elm uses, at: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
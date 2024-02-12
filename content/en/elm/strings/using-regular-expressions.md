---
title:                "Using regular expressions"
aliases: - /en/elm/using-regular-expressions.md
date:                  2024-02-03T19:02:47.206438-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using regular expressions"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) in programming are patterns used for matching character combinations in strings. In Elm, just like in other languages, programmers use regex for tasks like validating input, searching, and replacing text within strings due to their flexibility and efficiency.

## How to:
Elm does not have built-in regex functions in its core library, requiring the use of third-party libraries for these operations. One of the popular choices for working with regex is `elm/regex`. You can add it to your project using `elm install elm/regex`.

Here's how you can use `elm/regex` for a few common tasks:

### 1. Matching a pattern
To check if a string matches a pattern, you can use `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Example usage:
isAlphanumeric "Elm2023"     -- Output: True
isAlphanumeric "Elm 2023!"   -- Output: False
```

### 2. Finding all matches
To find all occurrences of a pattern within a string, you can use `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Example usage:
getWords "Elm is fun!"  -- Output: ["Elm", "is", "fun"]
```

### 3. Replacing text
To replace parts of a string that match a pattern, you use `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Example usage:
replaceElmWithHaskell "Learning Elm is fun!"  
-- Output: "Learning Haskell is fun!"
```

In these examples, `Regex.fromString` is used to compile a regex pattern, where `\b` matches word boundaries, and `\w` matches any word character. Always handle the `Maybe` result of `Regex.fromString` to safeguard against invalid regex patterns, typically using `Maybe.withDefault`.

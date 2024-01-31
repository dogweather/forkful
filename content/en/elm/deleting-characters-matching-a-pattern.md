---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:41:49.379883-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern means to wipe out specific sets of characters from text, based on rules (patterns). Programmers do this for text cleaning, data processing, or to simplify input before parsing.

## How to:
Elm doesn't natively support regex, but you can simulate character deletion. Here's an example using `String.filter` to remove digits from a string.

```Elm
import Browser
import Html exposing (text)

removeDigits : String -> String
removeDigits = String.filter (\char -> not (char >= '0' && char <= '9'))

main =
  text (removeDigits "Elm 0.19.1 is super 123 cool!")

-- Output: "Elm . is super  cool!"
```

## Deep Dive
Elm lacks regex as part of its core language, differing from many other languages. This design choice aligns with Elm's goals for simplicity and safety. Regex can be error-prone and hard to debug, but Elm advocates simpler string operations that cover many common use cases.

For cases where regex is truly needed, the implementation relies on JavaScript interop via ports. However, Elm encourages finding solutions within the language first. The `String` module provides functions like `filter`, `replace`, and `split` which cover a wide range of pattern-based text manipulation without introducing regex's complexity. 

## See Also
- [Elm String documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Practical Elm for a Busy Developer](https://korban.net/elm/book/) - Book that includes text manipulation utilities.

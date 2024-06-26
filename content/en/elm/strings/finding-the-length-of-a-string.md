---
date: 2024-01-20 17:47:17.443264-07:00
description: 'How to: In Elm, you use `String.length` to find out how many characters
  a string contains. Witness.'
lastmod: '2024-03-13T22:45:00.001268-06:00'
model: gpt-4-1106-preview
summary: In Elm, you use `String.length` to find out how many characters a string
  contains.
title: Finding the length of a string
weight: 7
---

## How to:
In Elm, you use `String.length` to find out how many characters a string contains. Witness:

```elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Hello, Elm!"))
  -- Output: "11"
```

## Deep Dive
Historically, string length functions have been crucial for memory management and text processing in languages with low-level access to data. Elm, being high level, abstracts these details, offering built-in functionality with `String.length`.

Two points worth noting:
1. Elm strings are UTF-16 encoded. `String.length` returns the number of UTF-16 code units, which can differ from the actual number of Unicode graphemes (user-perceived characters) in strings with complex characters.
2. There aren't built-in alternatives to `String.length` in Elm. If you need the number of graphemes, you might need a custom function that accounts for Unicode intricacies.

Internally, `String.length` iterates over the string data structure, counting elements. As a pure function, its output depends solely on input, maintaining Elm's functional programming ethos.

## See Also
- Elm’s official `String` documentation: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- UTF-16: [https://en.wikipedia.org/wiki/UTF-16](https://en.wikipedia.org/wiki/UTF-16)

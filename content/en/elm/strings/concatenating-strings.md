---
date: 2024-01-20 17:34:24.812334-07:00
description: "Concatenating strings means sticking two or more pieces of text together.\
  \ It's as basic and essential as using duct tape, allowing you to create new\u2026"
lastmod: '2024-03-13T22:45:00.002077-06:00'
model: gpt-4-1106-preview
summary: Concatenating strings means sticking two or more pieces of text together.
title: Concatenating strings
weight: 3
---

## How to:
Elm's got a neat operator `(++)` to save the day:

```Elm
greeting : String
greeting =
    "Hello, " ++ "world!"

-- "Hello, world!"
```

But sometimes, you've got a bunch of pieces. Fear not, `++` is chainable:

```Elm
fullName : String
fullName =
    "Elm" ++ " " ++ "Lang"

-- "Elm Lang"
```

And for lists of strings, `String.join` is your friend:

```Elm
words : List String
words =
    ["Join", "the", "Elm", "club"]

sentence : String
sentence =
    String.join " " words

-- "Join the Elm club"
```

## Deep Dive
Back in the day, you'd often concatenate strings with complex functions in other languages. In Elm, it's always been a breeze thanks to the `(++)` operator. If you're really concatenating a lot, efficiency could come into play; using `(++)` on long strings may be slower, because Elm has to walk through the entire string on the left of `(++)` every time.

There's also "interpolation" in some languages, but Elm doesn’t do string interpolation. No worries though, `(++)` and `String.join` have us covered.

Under the hood, when Elm concatenates, it tries to be smart about it, often using optimized JavaScript operations, which is what Elm compiles down to in the end. So even if `(++)` can seem simple, there's some cleverness going on behind the scenes to keep things zippy.

## See Also
- Elm official documentation on Strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Guide, where you can learn more about strings: https://guide.elm-lang.org/strings/

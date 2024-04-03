---
date: 2024-01-20 17:50:43.096950-07:00
description: "How to: Elm uses the `++` operator to concatenate strings, which you\
  \ can use for interpolation-like behavior. No special syntax; you just join them\u2026"
lastmod: '2024-03-13T22:44:59.996941-06:00'
model: gpt-4-1106-preview
summary: Elm uses the `++` operator to concatenate strings, which you can use for
  interpolation-like behavior.
title: Interpolating a string
weight: 8
---

## How to:
Elm uses the `++` operator to concatenate strings, which you can use for interpolation-like behavior. No special syntax; you just join them together.

```Elm
name = "world"
greeting = "Hello, " ++ name ++ "!"

-- Output
"Hello, world!"
```

## Deep Dive
Elm, emphasizing simplicity and maintainability, doesn't have built-in string interpolation like some other languages. Instead, you use `++` for string concatenation. Historically, string interpolation can be traced to early computing languages and has become more sophisticated over time. 

Alternatives in Elm could involve using functions to build up more complex strings, or using the `String.concat` or `String.join` functions if working with lists of strings. Custom functions could also be created to mimic interpolation syntax, but they won't be as clean as in languages with native support.

Under the hood, when you're using `++` to concatenate strings, Elm is efficiently creating a new string with the combined content. It's worth noting that overusing the `++` operator with large or numerous strings can be less efficient than methods in languages with native interpolation due to potential repeated copying of strings during concatenation.

## See Also
- Elm `String` Module Documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Syntax Overview: https://elm-lang.org/docs/syntax
- Elm Optimization Tips: https://elm-lang.org/0.19.1/optimization
- String Concatenation Discussion on Elm Discourse: https://discourse.elm-lang.org

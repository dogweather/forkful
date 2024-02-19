---
aliases:
- /en/elm/handling-errors/
date: 2024-01-21 21:19:20.721824-07:00
description: "Handling errors means writing code that can anticipate and deal with\
  \ things going wrong. Programmers do it to prevent crashes, protect data integrity,\
  \ and\u2026"
lastmod: 2024-02-18 23:09:10.979280
model: gpt-4-1106-preview
summary: "Handling errors means writing code that can anticipate and deal with things\
  \ going wrong. Programmers do it to prevent crashes, protect data integrity, and\u2026"
title: Handling errors
---

{{< edit_this_page >}}

## What & Why?
Handling errors means writing code that can anticipate and deal with things going wrong. Programmers do it to prevent crashes, protect data integrity, and provide users with graceful fallbacks.

## How to:
Elm’s core philosophy is No Runtime Exceptions. So, Elm leverages its type system with types like `Maybe` and `Result` to handle errors.

For `Maybe` scenario:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- When you run it:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

For `Result` scenario:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- And using it:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Deep Dive
Elm's type system is strict, which helps catch errors early. Historically, most languages relied on exceptions and runtime checks, but Elm chose compile-time guarantees. Alternatives like `Result` allow detailed error info, while `Maybe` is simpler for yes-no scenarios. Elm’s error handling encourages developers to consider all paths upfront, avoiding the pitfalls of forgotten error cases.

## See Also:
- Elm’s official guide section on error handling: [Error Handling – An Introduction](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` documentation: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` documentation: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)

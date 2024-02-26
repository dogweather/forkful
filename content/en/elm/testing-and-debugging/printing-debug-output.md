---
date: 2024-01-20 17:52:14.341987-07:00
description: "Printing debug output in Elm is about displaying values in the console\
  \ to understand what\u2019s happening in your code. We do it to catch bugs and ensure\
  \ the\u2026"
lastmod: '2024-02-25T18:49:56.457753-07:00'
model: gpt-4-1106-preview
summary: "Printing debug output in Elm is about displaying values in the console to\
  \ understand what\u2019s happening in your code. We do it to catch bugs and ensure\
  \ the\u2026"
title: Printing debug output
---

{{< edit_this_page >}}

## What & Why?

Printing debug output in Elm is about displaying values in the console to understand what’s happening in your code. We do it to catch bugs and ensure the logic flows as intended.

## How to:

Elm doesn't have a built-in `print` function like some languages, but you can use the `Debug` module for the console output:

```Elm
import Debug

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  model
    |> Debug.log "model before update"
    |> actualUpdateFunction msg
    |> Debug.log "model after update"
```

You'll see something like this in your browser's console:

```
model before update: { ... some model data ... }
model after update: { ... some updated model data ... }
```

Remember, the `Debug.log` function is handy, but don't ship your code with it. Elm will remind you to remove debug statements before you can make a production build.

## Deep Dive

`Debug.log` is part of the Elm `Debug` module, designed for development-time assistance only. Historically, Elm has emphasized a focus on maintainability and error handling, leaving the `Debug` module intentionally simple. Its simplicity ensures that developers keep an eye on meaningful output rather than getting lost in an extensive debugging suite.

Elm's `Debug.log` function takes two arguments: a string tag and the data to log out. The output is then printed to the browser console. The alternatives to this approach would be:

1. Traditional console logging: Elm doesn’t support direct console logging due to Elm's architecture aiming for zero runtime exceptions, and direct logging could break this guarantee.
2. Elm's Time-Traveling Debugger: This tool lets you visualize the state of your application over time without console logs and is a powerful way to debug complex apps.

Implementation-wise, the `Debug.log` function wraps your data with an identifier tag. This is useful to distinguish different data points. In production, the Elm compiler will flag any usage of `Debug.log`, ensuring you keep your production code clean from debugging artifacts.

## See Also

- Elm's official guide on debugging: https://guide.elm-lang.org/debugging/
- Time-Traveling Debugger introduction: https://elm-lang.org/news/the-perfect-bug-report
- Elm Debug module documentation: https://package.elm-lang.org/packages/elm/core/latest/Debug

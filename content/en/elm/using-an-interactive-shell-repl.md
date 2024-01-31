---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:27.205304-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
The Read-Eval-Print Loop (REPL) is a simple, interactive programming environment that takes single user inputs, evaluates them, and returns the result to the user. Elm programmers use REPL for quick experiments, debugging, or learning the language.

## How to:
Elm doesn't come with an integrated REPL. However, you can use `elm repl` from your command line to start an Elm session after installing Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

In this session, after importing List functions, we doubled the numbers in a list and got the result instantly.

## Deep Dive
Elm's REPL can seem limited compared to those of some other languages like Python or JavaScript, as Elm is a compiled language focused on producing web apps. Historically, Elm has focused on full applications rather than scripting or shell interactions.

Alternatives to Elm's REPL include `elm-live` and online editors like Ellie where you can see changes to code reflected in real-time in a browser.

Regarding implementation, the Elm REPL compiles snippets of Elm code into JavaScript in the background, allowing you to run Elm interactively. This is different from REPLs of interpreted languages, which don't need this compilation step. Elm REPL is also stripped down to keep the core language lightweight and focused.

## See Also
- Elm's official guide on interactivity: https://guide.elm-lang.org/interop/
- Ellie, an online Elm playground: https://ellie-app.com/new
- `elm-live`, a flexible dev server for Elm: https://www.elm-live.com/

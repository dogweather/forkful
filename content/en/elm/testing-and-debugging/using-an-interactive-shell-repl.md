---
title:                "Using an interactive shell (REPL)"
aliases:
- /en/elm/using-an-interactive-shell-repl.md
date:                  2024-01-25T03:39:27.205304-07:00
lastmod:               2024-01-31
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-31, dogweather, reviewed
---

{{< edit_this_page >}}

## What & Why?
The Read-Eval-Print Loop (REPL) is a simple, interactive programming environment that takes single user inputs, evaluates them, and returns the result to the user. Elm programmers use REPL for quick experiments, debugging, or learning the language.

## How to:
Elm comes with an integrated REPL. Use `elm repl` from your command line to start an Elm session:

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

In this session, after importing List functions, we doubled the numbers in a list and got the result instantly.

`elm repl --help` shows a little bit of information:

```
$ elm repl --help
The `repl` command opens up an interactive programming session:

    elm repl

Start working through <https://guide.elm-lang.org> to learn how to use this! It
has a whole chapter that uses the REPL for everything, so that is probably the
quickest way to get started.

You can customize this command with the following flags:

    --interpreter=<interpreter>
        Path to a alternate JS interpreter, like node or nodejs.

    --no-colors
        Turn off the colors in the REPL. This can help if you are having trouble
        reading the values. Some terminals use a custom color scheme that
        diverges significantly from the standard ANSI colors, so another path
        may be to pick a more standard color scheme.
```

## Deep Dive
Elm's REPL can seem limited compared to those of some other languages like Python or JavaScript, as Elm is a compiled language focused on producing web apps. Historically, Elm has focused on full applications rather than scripting or shell interactions.

Alternatives to Elm's REPL include `elm-live` and online editors like Ellie where you can see changes to code reflected in real-time in a browser.

Regarding implementation, the Elm REPL compiles snippets of Elm code into JavaScript in the background, allowing you to run Elm interactively. This is different from REPLs of interpreted languages, which don't need this compilation step. Elm REPL is also stripped down to keep the core language lightweight and focused.

## See Also
- Elm's official guide: https://guide.elm-lang.org/
- Ellie, an online Elm playground: https://ellie-app.com/new
- `elm-live`, a flexible dev server for Elm: https://www.elm-live.com/

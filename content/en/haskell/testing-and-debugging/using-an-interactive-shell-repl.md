---
title:                "Using an interactive shell (REPL)"
aliases:
- /en/haskell/using-an-interactive-shell-repl.md
date:                  2024-01-25T03:39:41.583758-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
An interactive shell, or REPL (Read-Eval-Print Loop), in Haskell lets you run code snippets live. It's a playground for quick feedback, testing functions, and learning the language.

## How to:
To start the GHCi (Glasgow Haskell Compiler's interactive environment), simply type `ghci` in your terminal. Here's how to use it:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Sample output explains that `x` is a numeric variable and shows that doubling it results in 10.

## Deep Dive:
Haskell's GHCi has come a long way since its inception. It provides a rich set of features like tab completion, multi-line input, and package loading. Alternatives like Hugs are mostly historical now, with GHCi being the standard. GHCi compiles code just-in-time each time you enter an expression, giving you an efficient way to test your Haskell code.

## See Also:
- [The GHC User's Guide – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Starting Out](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)

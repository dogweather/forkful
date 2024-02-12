---
title:                "Reading command line arguments"
aliases:
- /en/haskell/reading-command-line-arguments/
date:                  2024-01-20T17:55:55.048803-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Haskell lets you snatch user inputs when they run your program. Why? To customize program behavior on the fly, without altering the code itself.

## How to:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ show args ++ "!")
```

Run it passing "world" as an argument:

```bash
$ runhaskell yourprogram.hs world
Hello, ["world"]!
```

## Deep Dive

Haskell's a neat language, with roots in the 80s, favoring purity and static typing. It's had a way to handle command line arguments since the early days. In other languages, this might be pretty procedural stuff, but here, we're in the realm of IO monads to deal with the wild outside world.

Alternatives? You can go nuts with libraries like `optparse-applicative` for complex stuff, but for simple cases, `getArgs` does the trick.

Under the hood? `getArgs` is a function that dives into your system, fetches whatever followed the program name in the terminal, and hands you a list of strings. It's implemented in Haskell's base library, leaning on lower-level C functions to do the grunt work. Neat, right?

## See Also

- Going deeper with `getArgs`: [Hoogle on System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- Levels up in argument parsing: [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)

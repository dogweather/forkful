---
date: 2024-01-20 17:55:55.048803-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:45:00.142391-06:00'
model: gpt-4-1106-preview
summary: .
title: Reading command line arguments
weight: 23
---

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

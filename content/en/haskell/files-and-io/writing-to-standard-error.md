---
date: 2024-02-03 19:03:30.324317-07:00
description: 'How to: In Haskell, writing to stderr is straightforward with the base
  library''s `System.IO` module. Below is a basic example to demonstrate.'
lastmod: '2024-03-13T22:45:00.143228-06:00'
model: gpt-4-0125-preview
summary: In Haskell, writing to stderr is straightforward with the base library's
  `System.IO` module.
title: Writing to standard error
weight: 25
---

## How to:
In Haskell, writing to stderr is straightforward with the base library's `System.IO` module. Below is a basic example to demonstrate:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
```

The output of this program to stderr would be:

```
This is an error message.
```

If you're working in a more complex application, or if you need better control over logging (including errors), you might opt for a third-party library. One popular choice is `monad-logger` which integrates with the `mtl` style of Haskell programming. Here's a small snippet using `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "This is an error message using monad-logger."
```

When run, the `monad-logger` version similarly outputs an error message, but it's equipped with more context like timestamps or log levels, depending on the configuration:

```
[Error] This is an error message using monad-logger.
```

Both methods serve the purpose of writing to stderr, with the choice largely depending on your application's complexity and needs.

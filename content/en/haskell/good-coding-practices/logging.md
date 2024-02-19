---
aliases:
- /en/haskell/logging/
date: 2024-01-25 02:03:40.217197-07:00
description: "Logging in programming is essentially leaving a trail of breadcrumbs\
  \ in the form of recorded events or messages, which can be used to track what your\u2026"
lastmod: 2024-02-18 23:09:11.102510
model: gpt-4-1106-preview
summary: "Logging in programming is essentially leaving a trail of breadcrumbs in\
  \ the form of recorded events or messages, which can be used to track what your\u2026"
title: Logging
---

{{< edit_this_page >}}

## What & Why?
Logging in programming is essentially leaving a trail of breadcrumbs in the form of recorded events or messages, which can be used to track what your application is doing at any given moment. Programmers do it to debug issues, monitor system performance, and audit behavior for security and compliance reasons.

## How to:
In Haskell, logging can be implemented using libraries like `monad-logger` or `hslogger`. Here's a quick example using `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Starting the application..."
    liftIO $ putStrLn "Doing some critical work..."
    logErrorN "Oops! Something went wrong."

main :: IO ()
main = runStdoutLoggingT logExample

{- Sample Output
[Info] Starting the application...
Doing some critical work...
[Error] Oops! Something went wrong.
-}
```

This simple example demonstrates how you can sprinkle logging statements throughout your code to get insights on what's happening at runtime. `logInfoN` and `logErrorN` are used to log informational and error messages respectively.

## Deep Dive:
Logging has come a long way from simple print statements to sophisticated logging frameworks. Historically, logs were just text outputs to a console or file, but now they include structured data that can be parsed and analyzed by various tools.

In Haskell, logging can be done in a pure functional style that involves passing log actions explicitly or using monadic contexts for impurity, where loggers are implicitly threaded through computation.

The `hslogger` library, for example, is more traditional and mutable compared to `monad-logger`. `monad-logger` offers integration with the monad stack and provides more flexibility in terms of output formatting and control. Both libraries allow you to set log levels, which help in filtering log messages based on their importance. Log levels include debug, info, notice, warning, error, critical, alert, and emergency.

Haskell's approach to logging often aligns with its emphasis on type safety and purity. Logs can be handled in such a way that even if the logging fails, it won’t cause the main application to crash due to Haskell's robust error handling capabilities.

## See Also:
- [`monad-logger` documentation on Hackage](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` package on Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Chapter 19, on Error Handling](http://book.realworldhaskell.org/read/error-handling.html)
- [The Logging Facade for Haskell (log-base)](https://hackage.haskell.org/package/log-base)

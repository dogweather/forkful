---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output is the process of outputting code information for debugging purposes. Programmers do it to find and fix issues in their code by closely examining the step-by-step execution.

## How to:
In Haskell, you can use the `Debug.Trace` module to print debug output.

Here's the way to go:

```Haskell
import Debug.Trace

exampleFunction :: Int -> Int
exampleFunction x = trace ("Debug: x = " ++ show x) (x * x) 
```

In the example, when you call `exampleFunction 3`, it prints "Debug: x = 3" and returns 9. `trace` is useful when you want to inspect values in pure functions for debugging.

## Deep Dive
1. **Historical context**: In the initial phase of Haskell, debugging was quite a challenge due to its inherent purity. Later, with the `Debug.Trace` module, things improved. However, remember that debug tracing is not a "pure" function and impacts referential transparency, so use it judiciously.
2. **Alternatives**: For serious debugging, consider using a proper Haskell debugging tool like GHCi debugger, Hoed, or Haskell Debug Adapter.
3. **Implementation details**: The `trace` function relies on `unsafePerformIO` under the hood to print debug output to stderr. It's typically used temporarily during debugging and removed when the bug is resolved.

## See Also
1. [Haskell's Debug.Trace Documentation](http://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
2. [Hoed, A Haskell Debugging Tool](https://wiki.haskell.org/Hoed)
3. [Haskell Debug Adapter](https://github.com/phoityne/haskell-debug-adapter)
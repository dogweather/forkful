---
title:                "Printing debug output"
date:                  2024-01-20T17:52:42.606115-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is about spitting out data from your program to see what's going on under the hood. Programmers do it to track variables, understand flow, and squash pesky bugs.

## How to:

A straightforward way to print debug information in Haskell is with the `print` function, which takes a value that's an instance of the `Show` typeclass and outputs it to the console.

```Haskell
main :: IO ()
main = do
  let number = 42
  print number
  putStrLn "Debugging is a piece of cake in Haskell!"

-- Output:
-- 42
-- Debugging is a piece of cake in Haskell!
```

For more complex data structures, ensure they derive `Show` to enable pretty printing:

```Haskell
data Cake = Chocolate | Vanilla deriving Show

debugFlavor :: Cake -> IO ()
debugFlavor flavor = print flavor

main :: IO ()
main = debugFlavor Chocolate

-- Output:
-- Chocolate
```

Sometimes we want temporary debugging that's easy to remove later. Enter the `Debug.Trace` module.

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ trace "This will print first" "This will print second"

-- Output:
-- This will print first
-- This will print second
```

The `trace` function prints the string when the value is evaluated, but it's a side effect in the pure part of the code. It's handy but use with caution!

## Deep Dive

In ye olden days, debugging might've been the ol' "print statement" trick. Haskell offers this with a functional twist and tools for cleaner debug practices. Enter `print` and the `Debug.Trace` module, as explored earlier. 

Alternates to `print` include `putStrLn` for strings and `putStr`, if you're not into that automatic newline. `Debug.Trace` also has variants like `traceShow` that work directly with `Show` instances, saving you a `show` call.

As for implementation details, `print` is essentially `putStrLn . show`. It prints any `Show`-able data to stdout. `Debug.Trace` functions, on the other hand, are meant for temporary use during development. They sneak into pure code and violate referential transparency which is a no-no in the long run.

Don't forget logging libraries for serious applications, which offer more control and less "debug by print."

## See Also

- The `Debug.Trace` documentation: [https://hackage.haskell.org/package/base/docs/Debug-Trace.html](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- Haskell Wiki on Debugging: [https://wiki.haskell.org/Debugging](https://wiki.haskell.org/Debugging)
- A nice discussion on why not to use `Debug.Trace` and what to do instead: [https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice](https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice)

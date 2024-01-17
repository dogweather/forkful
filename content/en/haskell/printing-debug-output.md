---
title:                "Printing debug output"
html_title:           "Haskell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the process of displaying information about the state of a program during its execution. This is often used by programmers to track down bugs and optimize their code. Printing debug output can provide valuable insights into the program's behavior and help in identifying and resolving issues.

## How to:

To print debug output in Haskell, we can use the `trace` function from the `Debug.Trace` module. This function takes in a string as its first argument, which will be displayed as the debug output. We can then use this function anywhere in our code to print out useful information.

```Haskell
import Debug.Trace

-- simple example
let x = 5
trace "x is now: " x

-- output: x is now: 5
```

We can also use the `traceShow` function to print out the value of a variable along with a custom message.

```Haskell
import Debug.Trace

-- using traceShow
let x = 5
traceShow "x is now: " x

-- output: x is now: 5
```

## Deep Dive:

Historically, printing debug output has been a popular method for debugging and troubleshooting code. However, with advances in debugging tools and IDEs, it's becoming less common. Some alternative methods for debugging in Haskell include using the `Debug.Trace` module with GHCi or using a debugger such as GHCi's `-fbreak-on-error` option.

The `trace` function works by adding the debug output to the program's standard output. This means that it will also be included in the program's final output, which may not be desirable. To avoid this, we can use the `traceM` function, which discards the output and only prints it to the console. Another issue with using `trace` for debugging is its significant performance overhead, so it's recommended to remove the `trace` calls in a production-ready code.

## See Also:

- [Debugging in Haskell](https://wiki.haskell.org/Debugging)
- [GHCi Debugging Options Documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Proper Debugging in Haskell](https://blog.jez.io/debugging-in-haskell/)
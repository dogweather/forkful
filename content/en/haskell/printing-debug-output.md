---
title:    "Haskell recipe: Printing debug output"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language, and Haskell is no exception. When writing code, it's common to encounter errors or unexpected behavior that can be difficult to determine the source of. This is where printing debug output comes in handy. By printing out certain values or variables in your code, you can get a better understanding of what is happening and where the issue may be occurring.

## How To

Printing debug output in Haskell is a simple process. First, you can use the `Debug.Trace` module to access the `trace` function, which allows for printing strings along with any value you want to inspect. Let's take a look at an example.

```
Haskell
import Debug.Trace

main = do
  let x = 10
  let y = 5
  let z = x + y
  trace ("Value of x: " ++ show x) z
  trace ("Value of y: " ++ show y) z
```

Here, we are using `trace` to print out the value of `x` and `y` while also performing the addition operation. The `show` function converts the integer values into strings so they can be concatenated with our custom strings. The output of this code would look like this:

```
Value of x: 10
Value of y: 5
15
```

As you can see, the values of `x` and `y` are printed out before the final result of `15`. This is helpful in understanding what values are being used in the calculation.

## Deep Dive

The `Debug.Trace` module also provides additional functions such as `traceShow` and `traceStack`. These functions allow you to print out not only values but also the call stack of your code. This can be particularly useful in larger and more complex programs to pinpoint where an error may be occurring. It is important to note that the `Debug.Trace` module should only be used for debugging purposes and not in production code.

## See Also

- [Debugging in Haskell](https://www.haskell.org/haskellwiki/Debugging)
- [Haskell Debugging using ghci](https://wiki.haskell.org/Debugging)
- [Tracing in Haskell](https://markkarpov.com/post/tracing-in-haskell.html)
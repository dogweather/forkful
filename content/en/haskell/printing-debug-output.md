---
title:    "Haskell recipe: Printing debug output"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language, and Haskell is no exception. Printing debug output can help programmers track down the source of errors and make the debugging process smoother and more efficient.

## How To

To print debug output in Haskell, we can use the `Debug.Trace` module. Let's take a look at an example:

```Haskell
import Debug.Trace

factorial :: Int -> Int
factorial n
    | n < 0 = error "Number must be positive."
    | n == 0 = 1
    | otherwise = n * factorial (n-1)

main = do
    let result = factorial 5
    traceM $ "The factorial of 5 is " ++ show result
    putStrLn $ "The result is: " ++ show result
```

In the code above, we import the `Debug.Trace` module and use the `traceM` function to print the debug message. We can use the `show` function to convert the result into a string and concatenate it with our debug message. Running this code will output the following:

```
The factorial of 5 is 120
The result is: 120
```

We can also use `trace` instead of `traceM` if we don't want to print the result. Additionally, we can use `traceShow` if we want to print both the debug message and the value of the expression being traced. 

## Deep Dive

The `Debug.Trace` module provides a variety of functions for printing debug output, including `trace`, `traceM`, and `traceShow`, as mentioned above. It also has functions for conditional tracing, such as `traceIf` and `tracePredicate`.

It's important to note that the `Debug.Trace` module should only be used for debugging purposes and not in production code. This is because the debug output can significantly slow down the performance of your program. Therefore, it's recommended to remove all traces before deploying your code.

Another useful technique when using `Debug.Trace` is to use it with the `Debug` language pragma. This pragma allows us to add debug statements without having to import the `Debug.Trace` module in every file. 

## See Also

- [Haskell Debugging Guide](https://www.haskell.org/debugging/)
- [Haskell Language Pragmas Documentation](https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html#language-pragma)
- [Debugging in Haskell Using Trace and Debug.Trace](https://drek4537.github.io/A-Simple-Overview-of-Haskell/module03/18-debugging-part-1-b.html)
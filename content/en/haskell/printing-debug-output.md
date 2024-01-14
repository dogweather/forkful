---
title:                "Haskell recipe: Printing debug output"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

As developers, we often encounter bugs and errors in our code. While unit testing and debugging tools can help track down these issues, sometimes it's simply easier to print out debug output to get a better understanding of what is happening in our code. In this blog post, we will discuss how to do just that in Haskell, a powerful functional programming language.

## How To

To print out debug output in Haskell, we can use the `trace` function from the `Debug.Trace` module. The `trace` function takes in a string as input and returns the same string as output while also printing it to the console. Let's take a look at an example:

```Haskell
import Debug.Trace

myFunction :: Int -> Int
myFunction x = trace ("The input value is: " ++ show x) (x * 2)
```
If we call `myFunction` with an input of 5, we will see the following output in the console:

```
"The input value is: 5"
```

We can also use the `traceShow` function to print out the result of an expression. Let's modify our previous example to use `traceShow` instead:

```Haskell
import Debug.Trace

myFunction :: Int -> Int
myFunction x = traceShow (x * 2) (x * 2)
```

Now, when we call `myFunction` with an input of 5, we will see the following output in the console:

```
10
```

## Deep Dive

While using `trace` and `traceShow` can be useful for simple debugging purposes, it's important to note that these functions are meant for debugging only and should not be used in production code. This is because these functions can have unexpected side effects and can affect the performance of our code.

Another thing to keep in mind is that each time we call `trace` or `traceShow`, a string is created and stored in memory. This can be a problem if we are using these functions in a recursive function, as it can quickly lead to memory consumption and potential performance issues.

## See Also

- [Haskell Debugging Guide](https://wiki.haskell.org/Debugging)
- [Debug.Trace documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)

By using the `trace` and `traceShow` functions, we can easily print out debug output in Haskell and gain a better understanding of our code. Just remember to use these functions with caution and only for debugging purposes. Happy coding!
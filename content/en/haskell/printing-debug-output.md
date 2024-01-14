---
title:                "Haskell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why: 

As programmers, it's important to have a way to debug our code and understand what's happening under the hood. Printing debug output is a useful tool to help with this process and identify any errors or bugs in our programs.

## How To: 

Printing debug output in Haskell is quite simple. We can use the `trace` function from the `Debug.Trace` module. Let's take a look at a quick example:

```Haskell
import Debug.Trace

-- A simple function that returns the sum of two numbers
add :: Int -> Int -> Int
add x y = x + y

-- We can use trace to print out the value of our variables
main = do
  let x = 5
  let y = 10
  let sum = add x y
  trace ("The value of x is: " ++ show x) (return ())
  trace ("The value of y is: " ++ show y) (return ())
  trace ("The sum of x and y is: " ++ show sum) (return ())
```

When we run this code, we will see the following output:

```
The value of x is: 5
The value of y is: 10
The sum of x and y is: 15
```

As we can see, the `trace` function allows us to print out the values of our variables during runtime. This can be extremely helpful in understanding how our code is executing and identifying any potential issues.

## Deep Dive: 

The `trace` function works by taking in a message and a value and printing them both to the console during execution. It then returns the given value, allowing us to use it in the rest of our code. In the example above, we used `return ()` since we didn't need to use the value for anything else.

One thing to keep in mind when using `trace` is that it should only be used for debugging purposes. It is not recommended to use it in production code, as it can cause performance issues.

We can also use the `traceShow` function from the `Debug.Trace` module to print out the result of an expression. For example:

```Haskell
import Debug.Trace

-- A function that checks if a number is even
isEven :: Int -> Bool
isEven x = traceShow (x `mod` 2 == 0) (x `mod` 2 == 0)
```

When we call this function, we will see the following output:

```
True
```

This can be useful when we want to quickly check the result of an expression without cluttering our code with `trace` statements.

## See Also:

- [Debugging in Haskell](https://wiki.haskell.org/Debugging)
- [Using trace to debug a Haskell application](https://www.ryadel.com/en/haskell-using-trace-debug-application-example-tutorial/)
- [Debugging monadic code in Haskell](https://blog.jez.io/2017/05/31/haskell-debugging-monadic-code/)
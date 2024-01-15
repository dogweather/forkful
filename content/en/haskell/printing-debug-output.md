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

## Why

Debug output is a critical tool for programmers to identify and fix issues in their code. By printing out the values of specific variables or steps in the program, developers can better understand the flow of their code and catch any unexpected behavior.

## How To

Printing debug output in Haskell is simple and can be done using the `trace` function from the `Debug.Trace` module. Let's look at an example:

```Haskell
import Debug.Trace

-- A function that adds two numbers
add :: Int -> Int -> Int
add x y = trace ("Adding " ++ show x ++ " and " ++ show y) (x + y)
```

In the above code, we have imported the `Debug.Trace` module and defined a function `add` that takes in two integers and returns the sum of those numbers. However, we have also used the `trace` function to print out a message before returning the result. Let's see the output when we call this function:

```Haskell
> add 2 3
Adding 2 and 3
5
```

As you can see, the message we provided in `trace` is printed out before the result. This can be extremely helpful when debugging complex functions or when unexpected results are obtained.

Another useful feature of `trace` is that it can also print out the result of an expression. Let's modify our previous example to showcase this:

```Haskell
-- A function that multiplies two numbers
multiply :: Int -> Int -> Int
multiply x y = trace ("Multiplying " ++ show x ++ " and " ++ show y ++ " = " ++ show (x*y)) (x * y)
```

Now when we call this function, we will get the multiplication result printed out along with our custom message:

```Haskell
> multiply 5 6
Multiplying 5 and 6 = 30
30
```

## Deep Dive

Behind the scenes, the `trace` function works by inserting a value into the program's IO stream. This value is then printed out in the standard output. However, this also means that the `trace` function should only be used for debugging purposes and not for production code. This is because printing to the standard output can have a significant impact on the performance of the program.

Additionally, the `trace` function can only output strings. So if you need to print out the value of a non-string variable, you will need to use the `show` function to convert it into a string.

## See Also

- [Haskell Debugging and Tracing](https://wiki.haskell.org/Debugging_and_tracing)
- [Debugging in Haskell with `import Debug.Trace`](https://www.fpcomplete.com/blog/2017/06/debugging-in-haskell-with-import-meaning-debug-trace/)
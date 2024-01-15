---
title:                "Reading command line arguments"
html_title:           "Haskell recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to Haskell, you may be wondering why you would need to learn about reading command line arguments. Well, being able to read and process command line arguments is a crucial skill for building command line applications and scripts. It allows for user input to be passed directly into your code, making your programs more dynamic and interactive.

## How To

To start reading command line arguments in Haskell, we first need to import the `System.Environment` module. This module provides functions for interacting with the command line environment.

```Haskell
import System.Environment (getArgs)
```

Next, we can use the `getArgs` function to retrieve a list of the arguments passed into our program. This list will consist of strings, with each element representing a single argument.

```Haskell
main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Command line arguments: " ++ show args)
```

If we run this program with the command line arguments `Hello World`, the output will be:

```
Command line arguments: ["Hello", "World"]
```

We can also use the `head` function to retrieve the first argument in the list, and then convert it to a different data type if needed. For example, if we wanted to convert the first argument to an integer, we could do the following:

```Haskell
main :: IO ()
main = do
  args <- getArgs
  let firstArg = head args
      intArg = read firstArg :: Int
  putStrLn ("Integer argument: " ++ show intArg)
```

If we run this program with the command line argument `123`, the output will be:

```
Integer argument: 123
```

## Deep Dive

Let's dive a little deeper into the `getArgs` function. This function uses the `IO` monad, which means that it performs an input-output action and returns a value. In this case, the value returned is a list of strings. The function works by accessing the special `argv` variable, which contains the arguments passed into the program.

It's also important to note that the `getArgs` function only captures arguments passed directly into the program. Arguments passed after a `--` will be ignored. For example, if we run our program with the command line arguments `--verbose Hello`, the output will only show `["Hello"]`.

## See Also

For more information on command line arguments in Haskell, check out these resources:

- [Official documentation for System.Environment module](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- [Learn You a Haskell - I/O Chapter](http://learnyouahaskell.com/input-and-output)
- [Haskell for all - Command Line Arguments](https://www.haskellforall.com/2021/01/haskell-command-line-arguments.html)
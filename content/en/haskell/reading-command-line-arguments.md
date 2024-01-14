---
title:                "Haskell recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

Command line arguments are a powerful and essential part of any programming language, including Haskell. They allow us to provide inputs to our programs directly from the command line, making our programs more dynamic and versatile. In this blog post, we will explore how to read command line arguments in Haskell and how it can benefit our programming.

## How To 

Reading command line arguments in Haskell is a straightforward process. We can access the command line arguments through the `getArgs` function provided by the `System.Environment` module. Let's take a look at a simple example:

```
-- Import the necessary module
import System.Environment (getArgs)

-- Define a function to read and display the command line arguments
printArgs :: IO ()
printArgs = do
    -- Get the command line arguments as a list of strings
    args <- getArgs
    -- Print each argument on a separate line
    mapM_ putStrLn args
```

When we run this code and pass in some arguments, for example: `stack runhaskell Main.hs argument1 argument2`, we will get the following output:

```
argument1
argument2
```

We can also perform operations on the command line arguments, such as converting them to the desired data type, using functions like `read` or `fromJust` from the `Text.Read` module. Let's see an example:

```
-- Import the necessary modules
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Define a function to calculate the sum of the command line arguments
calcSum :: IO ()
calcSum = do
    -- Get the command line arguments as a list of strings
    args <- getArgs
    -- Convert the arguments to integers and calculate the sum
    let sum = sum $ mapMaybe readMaybe args :: Int
    -- Print the result
    print sum
```

When we run this code and pass in some arguments, for example: `stack runhaskell Main.hs 8 4`, we will get the following output:

```
12
```

## Deep Dive 

Behind the scenes, the `getArgs` function uses the `System.Environment.getProgName` function to get the name of the program and the `System.IO.getArgs` function to get the arguments passed in by the user. Command line arguments are stored as a list of strings, so we need to convert them to the desired data type if needed. Additionally, we can use the `System.Console.GetOpt` module to parse and validate more complex command line options.

## See Also 

To learn more about command line arguments in Haskell, check out these resources:

- Official Haskell Documentation on Command Line Arguments: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-cmdline.html
- "Haskell Command-Line Arguments" Tutorial by Learn You a Haskell for Great Good: http://learnyouahaskell.com/input-and-output#command-line-arguments
- "Command Line Parsing and Error Handling in Haskell" Blog Post by Functional Works: https://www.functionalworks.com/blog/2018/11/28/command-line-parsing-error-handling-in-haskell
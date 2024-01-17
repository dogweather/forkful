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

## What & Why?
Command line arguments are inputs provided by the user through the command line interface when running a program. Programmers use this feature to dynamically provide specific values to their programs at runtime, making them more versatile and customizable.

## How to:
To read command line arguments in Haskell, we use the `getArgs` function from the `System.Environment` module. This function returns a list of strings representing the arguments passed in. For example, if we run the following code in the command line:
```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Hello " ++ head args ++ "!"
```
And provide our name as an argument:
```
$ runhaskell hello.hs John
```
The output will be:
```
Hello John!
```

## Deep Dive:
Historically, command line arguments were used as a way to pass options and parameters to programs in the pre-graphical user interface (GUI) era. Alternative ways of providing inputs, such as through a configuration file, have also been used. The implementation for reading command line arguments in Haskell utilizes the `getProgName` function to return the program's name and `getArgs` to parse the arguments.

## See Also:
- [Haskell getArgs documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#v:getArgs)
- [Command line arguments on Wikipedia](https://en.wikipedia.org/wiki/Command-line_argument)
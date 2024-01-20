---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is when your program fetches input directly from the terminal command you used to run the program. You'd do this to make your program's behavior customizable without altering the source code.

## How to:

Let's dive right in. We'll use `System.Environment`'s `getArgs` function. Here's a simple program that echoes the command line arguments:

```Haskell
import System.Environment
main :: IO ()
main = do
  args <- getArgs
  print args
```
Run the code with arguments, and they'll get printed:

```Haskell
$ runghc Test.hs arg1 arg2 arg3
["arg1","arg2","arg3"]
```
Simple as that. We're grabbing command line arguments and printing them, and they come as a list of strings.

## Deep Dive

Historically, command line arguments in functional languages were an early form of parameterization, hailing back to times when UIs were terminal-based.

Alternatives to `getArgs` include `getProgName` and `getEnv`. The former fetches the name of the running program, and the latter lets you access the system's environmental variables.

`getArgs` employs lazy IO internally, so it can handle huge amounts of command line input without bloating memory usage.

## See Also

Official GHC documentation on `System.Environment`: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html

Learn about lazy IO: https://wiki.haskell.org/Lazy_IO

How CLI argument parsing looks in bash to compare: https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php
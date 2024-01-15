---
title:                "Writing to standard error"
html_title:           "Haskell recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

If you're a Haskell developer, you may have come across the need to write to standard error while debugging or logging your code. Writing to standard error allows you to output information or error messages to the terminal, making it a useful tool for troubleshooting and monitoring the execution of your program.

## How To

To write to standard error in Haskell, we use the `hPutStrLn` function from the `System.IO` module. It takes two parameters - a handle and a string - and outputs the string to the specified handle. In this case, we want to output to the standard error handle, which is represented by `stderr`.

First, we need to import the `System.IO` module into our Haskell file:

```Haskell
import System.IO
```

Then, we can use the `hPutStrLn` function to write to standard error:

```Haskell
hPutStrLn stderr "An error has occurred."
```

If we run the program, we will see the string "An error has occurred." printed to the terminal. This shows us how we can use the `hPutStrLn` function to write to the standard error handle in Haskell. 

## Deep Dive

In Haskell, the standard error handle `stderr` is a predefined value from the `System.IO` module. It is a handle that represents the standard error output stream, and is used for writing error messages or information that needs to be displayed to the user.

One important thing to note is that the standard error output is separate from the standard output (represented by `stdout`). This means that any output written to `stderr` will not interfere with the normal output of the program.

In addition to `hPutStrLn`, there are other functions in the `System.IO` module that can be used to write to standard error, such as `hPutStr` and `hPutChar`. These functions provide different ways of formatting and outputting data to `stderr`. 

## See Also

- [Haskell Documentation: System.IO Module](https://www.haskell.org/documentation/#system-io)
- [Writing to Standard Error in Haskell](https://jelv.is/blog/Writing-to-standard-error-in-Haskell/)
- [Handling Errors in Haskell](https://en.wikibooks.org/wiki/Haskell/Handling_errors)
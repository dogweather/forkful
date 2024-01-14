---
title:                "Haskell recipe: Writing to standard error"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When programming in Haskell, one may come across the need to write to standard error instead of standard output. This could be useful for debugging purposes or for displaying error messages to the user. In this blog post, we will explore how to write to standard error using Haskell.

## How To

To write to standard error in Haskell, we can use the `hPutStr` or `hPutStrLn` functions from the `System.IO` module. These functions take in a handle and a string as arguments.

A handle represents a stream of data, and in this case, it represents standard error. We can use the `stderr` function to get the handle for standard error.

Let's take a look at an example:

```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    hPutStrLn stderr "This will be written to standard error."
```

Running this code will print the given string to the standard error stream. The output will look something like this:

```
This will be written to standard error.
```

We can also use the `hPutStr` function to write to standard error without adding a new line at the end of the string. Here's an example:

```Haskell
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
    hPutStr stderr "This will be written"
    hPutStr stderr " to standard error."
```

The output of this code will be:

```
This will be written to standard error.
```

## Deep Dive

Under the hood, the `hPutStr` and `hPutStrLn` functions make use of buffers to improve performance. A buffer is a temporary storage area for data before it is written to a stream.

When we use the `hPutStr` or `hPutStrLn` functions, the string is first stored in a buffer and then written to standard error. This allows the program to write multiple strings to standard error without constantly accessing the actual stream.

If we want to manually flush the buffer and force the program to write the contents to the stream, we can use the `hFlush` function. Here's an example:

```Haskell
import System.IO (hPutStrLn, hFlush, stderr)

main :: IO ()
main = do
    hPutStrLn stderr "This will be written to standard error."
    hFlush stderr
    hPutStrLn stderr "This string will also be written to standard error."
```

The `hFlush` function clears the buffer for standard error, so when we use `hPutStrLn` again, the string will be written to standard error immediately instead of waiting for the buffer to fill up or for the program to end.

## See Also

Here are some other helpful resources for writing to standard error using Haskell:

- [Haskell documentation for System.IO module](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [A tutorial on I/O in Haskell](https://www.haskelltutorial.com/io.html)
- [A video tutorial on handling I/O in Haskell](https://www.youtube.com/watch?v=Vgu82wiiZ90)
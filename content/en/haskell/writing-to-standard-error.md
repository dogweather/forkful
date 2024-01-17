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

## What & Why?

Writing to standard error, also known as stderr, is a way for programmers to print error messages or other relevant information to the console. It allows for separate output from regular print statements and can be useful for debugging purposes.

## How to:

```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    let x = 5
    if x > 10
        then putStrLn "x is greater than 10"
        else hPutStrLn stderr "x is not greater than 10"
```

Output:
```
x is not greater than 10
```

## Deep Dive:

Writing to stderr has been a common practice since the early days of programming languages, providing a way for developers to distinguish between different types of output and effectively communicate important information to the user. While it may seem similar to writing to standard output, or stdout, there is a key difference. The stdout stream is typically used for regular program output, while stderr is reserved for error messages and other types of diagnostic information.

In Haskell, the `System.IO` module provides functions such as `hPutStrLn` to write to different output streams. However, there are also other methods for printing error messages, such as using the `error` function or throwing exceptions.

## See Also:

For more information on writing to different output streams in Haskell, check out the [System.IO documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html) and the [Control.Exception module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html).
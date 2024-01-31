---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) in Haskell lets you report errors and debug info separate from standard output (stdout). It's done to keep output streams organized, making it easier to handle only what's needed—like piping output or logging errors.

## How to:

Use System.IO to write to stderr. Here's a simple demo:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This will go to stderr"
  putStrLn "This will go to stdout"
```

Output when running the program:

```
This will go to stdout
```

To see the stderr output, redirect it:

```bash
runhaskell your_program.hs 2> error.log
```

`error.log` now contains "This will go to stderr".

## Deep Dive

Haskell's IO system differentiates between stdout and stderr, maintaining Unix conventions. Before Haskell, languages like C set the precedent of separated streams—stdout for results, stderr for errors and logs.

Alternative ways to output include using libraries like `System.Log.Logger` for more complex logging. Regarding implementation, stderr in Haskell is an `Handle`, just like a file handle, but predefined to refer to the system’s error output.

## See Also

- [Haskell System.IO library](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html): Detailed docs on System.IO.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/): An introductory Haskell book that covers I/O.

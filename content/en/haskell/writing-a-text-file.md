---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
In Haskell, writing a text file is all about saving data to a file. Programmers do this to persist data between sessions, share information, or log program output.

## How to:
Writing text files in Haskell is straightforward. Here's the gist using `writeFile`:

```Haskell
import System.IO

main :: IO ()
main = do
  let content = "Hello, file!"
  writeFile "greetings.txt" content
```

This code generates a file `greetings.txt` with "Hello, file!" inside.

For appending text, use `appendFile`:

```Haskell
appendToFile :: FilePath -> String -> IO ()
appendToFile file content = appendFile file content

-- Usage
main :: IO ()
main = appendToFile "greetings.txt" "\nSee you soon!"
```

Now, `greetings.txt` will also have "See you soon!" at the end.

## Deep Dive
Haskell's file writing functions stem from its robust IO handling. `writeFile` and `appendFile` are convenient wrappers around lower-level operations. Alternatives like `hPutStr` or `hPutStrLn` provide more control, letting us specify a file handle.

Details:
- `writeFile`: truncates the file before writing.
- `appendFile`: doesn't truncate, just adds to the end.
- Both handle text encoding and buffering.
- For non-text data, use functions like `hPutBuf`.

## See Also
For more info and best practices:

- [Haskell Documentation on IO](https://haskell.org/documentation)
- [Learn You a Haskell for Great Good! - IO](http://learnyouahaskell.com/input-and-output)
- [Real World Haskell - Working with Files](http://book.realworldhaskell.org/read/io.html)

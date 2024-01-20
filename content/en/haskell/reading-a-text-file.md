---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Haskell: Reading Text Files Straight to the Point

## What & Why?

Reading a text file is the act of pulling data from a text file into memory. Programmers do this to manipulate, analyze, and transform file data efficiently.

## How to:

Reading a file in Haskell is easy. Let's look at a simple sample.

```Haskell
main :: IO ()
main = do
    contents <- readFile "test.txt"
    putStrLn contents
```
In this example, readFile reads "test.txt" from your current directory and takes its content as a string. The putStrLn function then writes that string to standard output.

This gives an output like:

```Shell
Hello from Haskell!
```
Assumed that 'test.txt' has this content.

## Deep Dive

While Haskell's modern file reading functions seem straightforward, they've evolved subtly over its three-decade history. For example, 'readFile' now leverages 'lazy IO'. It doesn't read the entire file into memory upfront but instead reads chunks as needed, optimizing memory use.

There are alternatives to readFile. For large files, consider using the conduit or pipes libraries. These provide more controlled, efficient data streaming.

While `readFile` does an excellent job for ASCII or UTF-8 files, it might hiccup if you're working with different character encodings. The `text` library offers `Data.Text.IO.readFile`, which handles this better.

```Haskell
import qualified Data.Text.IO as Text

main :: IO ()
main = do
    contents <- Text.readFile "test.txt"
    putStrLn (Text.unpack contents)
```

It's the same as our first code snippet, but now it can better handle diverse text encodings.

## See Also 

Groovy guidance on exemplary Haskell programming library 'text':
- https://hackage.haskell.org/package/text

In-depth exploration of 'conduit' and 'pipes', two great libraries for masters of large file manipulation:
- https://haskell-lang.org/library/conduit
- https://haskellpipes.com/


Expand your Haskell knowledge with the official documentation:
- https://www.haskell.org/
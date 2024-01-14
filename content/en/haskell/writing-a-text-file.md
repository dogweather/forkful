---
title:                "Haskell recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Before we dive into the details of how to write a text file in Haskell, let's first understand why one would engage in doing so. Writing a text file allows you to store, organize, and manipulate data in a structured format. This can be useful for a variety of purposes such as data analysis, creating configuration files, or even writing a basic report.

## How To
To write a text file in Haskell, we first need to import the `System.IO` module. This module provides functions for performing input and output operations. Next, we use the `openFile` function to open a file in write mode, which creates a new file if it doesn't exist or overwrites the existing file. Here is an example code snippet:

```Haskell
import System.IO

main = do
    file <- openFile "output.txt" WriteMode
    hPutStrLn file "This is a sample line."
    hPutStrLn file "This is another line."
    hClose file
```

In this code, we create a new file named "output.txt" and write two lines of text to it using the `hPutStrLn` function. Lastly, we close the file using the `hClose` function. If we run this code, a new file named "output.txt" will be created in the same directory as the Haskell file, and it will contain the following lines:

```
This is a sample line.
This is another line.
```

Alternatively, we can use the `withFile` function, which takes care of opening and closing the file for us. Here is an example of how it can be used:

```Haskell
import System.IO

main = do
    withFile "output.txt" WriteMode $ \file -> do
        hPutStrLn file "This is a sample line."
        hPutStrLn file "This is another line."
```

## Deep Dive
Now, let's take a deeper look at the `withFile` function. It takes three arguments: the file path, the file mode, and a function that takes a `Handle` as a parameter. The `Handle` is used to perform operations on the file, such as reading or writing. In the example above, we used the `WriteMode` to specify that we want to write to the file. Other modes include `ReadMode` for reading from a file and `AppendMode` for appending to an existing file. 

The `Handle` also has two important functions: `hGetContents` and `hPutStrLn`. The `hGetContents` function reads the entire file and returns its contents as a `String`. The `hPutStrLn` function takes a `Handle` and a `String` and writes the string to the file, followed by a newline character. 

In addition, the `hClose` function is used to close the file and flush any buffered output. This is important to ensure that all the data has been written to the file before it is closed.

## See Also
For more information on writing text files in Haskell, check out these resources:

- [Haskell I/O](https://www.haskell.org/tutorial/io.html)
- [File Handling in Haskell](https://www.tutorialspoint.com/haskell/haskell_file_handling.htm)
- [Real World Haskell: File IO](http://book.realworldhaskell.org/read/io.html)
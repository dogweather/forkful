---
title:                "Haskell recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a common way of storing data, and as a Haskell programmer, knowing how to read and manipulate them is a useful skill to have. Whether you're working with large datasets or just need to parse some simple text files, understanding how to read text files in Haskell can greatly improve your productivity.

## How To

Reading a text file in Haskell is a straightforward process that involves a few simple steps. First, we need to import the necessary libraries:

```Haskell
import System.IO
import Data.List
```

Next, we need to declare a file handle using the `openFile` function. This function takes in two parameters: the path to the file and a `ReadMode` flag indicating that we want to read the file.

```Haskell
fileHandle <- openFile "example.txt" ReadMode
```

Once we have our file handle, we can use the `hGetContents` function to read the contents of the file. This function returns a lazy stream of characters, which we can transform into a list of lines using the `lines` function.

```Haskell
fileContents <- hGetContents fileHandle
let lines = lines fileContents
```

We can then print out each line of the file using the `putStrLn` function.

```Haskell
mapM_ putStrLn lines
```

And that's it! We have successfully read a text file in Haskell.

## Deep Dive

When reading text files in Haskell, there are a few important things to keep in mind. First, lazy evaluation can be very useful when working with large files. By using the `hGetContents` function, we only read in as much of the file as needed, instead of loading the entire contents into memory.

Another aspect to consider is error handling. The `openFile` function can throw an exception if the file does not exist or if it cannot be opened for some reason. It's important to handle these exceptions properly, either by using the `try` function or by explicitly handling the possible error cases.

Lastly, using the `ByteString` library can greatly improve the efficiency of reading text files. This library provides a more efficient way of handling large amounts of data, which can be especially useful when working with large text files.

## See Also
- [Haskell IO documentation](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell ByteString documentation](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#t:ByteString)
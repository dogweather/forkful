---
title:    "Haskell recipe: Reading a text file"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Text files are a common and important way of storing data in many programming languages, including Haskell. Whether you are creating a simple text-based game or processing large amounts of information, knowing how to read and manipulate text files can greatly enhance your skills as a Haskell programmer.

## How To

Assuming you have basic knowledge of Haskell and its syntax, let's dive into how to read a text file. First, we need to import the necessary library, `System.IO`, which contains functions for handling input and output operations.

```Haskell
import System.IO
```

Next, we need to create a file object using the `openFile` function, which takes in two parameters: the path to the file and the mode in which we want to open it. The modes available are `ReadMode`, `WriteMode`, `AppendMode`, and `ReadWriteMode`.

```Haskell
file <- openFile "sample.txt" ReadMode
```

Once we have our file object, we can use the `hGetContents` function to read the entire contents of the file as a string.

```Haskell
fileData <- hGetContents file
```

We can also use the `hGetLine` function to read a single line from the file.

```Haskell
line <- hGetLine file
```

Finally, we need to close the file to avoid any potential memory leaks using the `hClose` function.

```Haskell
hClose file
```

To see the output of our file reading, we can simply print the `fileData` or `line` variable to the console.

```Haskell
print fileData
-- or
print line
```

## Deep Dive

To take our file reading skills to the next level, it's important to understand how text files are read in Haskell. When we use the `openFile` function, it returns a `Handle` data type, which represents the connection between our program and the file.

The `hGetContents` and `hGetLine` functions use lazy evaluation, meaning that they don't read the entire contents of the file at once. Instead, they return a promise to provide the data when needed. This allows us to process large files without overloading our system's memory.

Another important thing to note is that the `Handle` object is immutable, meaning we can't change the file pointer once it's created. However, we can use the `hSeek` function to change the position of the file pointer if needed.

## See Also

- [Official Haskell documentation on file I/O](https://www.haskell.org/onlinereport/io.html)
- [Learn You a Haskell for Great Good! - File I/O](http://learnyouahaskell.com/input-and-output#files-and-streams)
- [Haskell for Mac - File I/O Tutorial](https://www.haskellformac.com/blog/file-io-tutorial/)
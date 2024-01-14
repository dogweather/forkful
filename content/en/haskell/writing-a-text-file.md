---
title:                "Haskell recipe: Writing a text file"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file in Haskell may seem like a simple task, but it can actually have many practical uses. From creating simple documentation to storing data in a format that can be easily read and manipulated, understanding how to write a text file in Haskell can greatly expand your programming capabilities.

## How To

To write a text file in Haskell, we will be using the `writeFile` function from the `System.IO` library. First, we need to open a new file or overwrite an existing one using the `openFile` function. Then, we can use the `writeFile` function to actually write the data to the file. Here is an example of how it looks in code:

```Haskell
import System.IO

main = do
  let fileName = "newfile.txt" -- name of the file we want to create
  handle <- openFile fileName WriteMode -- open the file in WriteMode
  writeFile handle "This is a new text file!" -- write the data to the file
  hClose handle -- close the file handle
```

In the above code, we first import the necessary library and then use the `openFile` function to create a file named "newfile.txt" in WriteMode. Then, we use the `writeFile` function to write the data "This is a new text file!" to the file. Finally, we close the file handle using the `hClose` function.

Running this code will create a new text file named "newfile.txt" with the content "This is a new text file!". You can also use string interpolation to write dynamic data to the file, such as variables or functions.

## Deep Dive

Now that we know the basic steps for writing a text file in Haskell, let's dive deeper into the `writeFile` function. This function takes in two parameters - the file handle and the data to be written. It automatically converts the data to a string and writes it to the file, making it convenient and efficient for writing text files.

However, it's important to note that the data must be a string in order for the function to work. If you want to write other data types, you will need to convert them to strings first. Additionally, you can also use the `appendFile` function to add new data to an existing text file without overwriting its contents.

## See Also
- [Haskell Wiki on File IO](https://wiki.haskell.org/IO/File_IO)
- [Official Documentation for System.IO Library](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [A Beginner's Guide to Haskell IO](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_IO)

Writing a text file in Haskell may seem daunting at first, but with the knowledge of these basic concepts and functions, you can easily create and manipulate text files to enhance your programming projects. Happy coding!
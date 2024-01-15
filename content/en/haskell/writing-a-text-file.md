---
title:                "Writing a text file"
html_title:           "Haskell recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Before we dive into the technical aspects of writing a text file in Haskell, let's first understand why someone would want to do this. Text files are a common way to store and transfer data in a human-readable format. They can be used for a variety of purposes such as storing user input, program output, or even as a means of communication between different programs. Being able to write and manipulate text files is a useful skill for any programmer, and Haskell provides some unique features and functionality to make this process easier.

## How To

To start writing a text file in Haskell, we first need to import the necessary libraries. In this case, we will be using the `System.IO` library, which provides functions for working with input and output. We can do this by adding the following line at the beginning of our file:

```Haskell
import System.IO 
```

Next, we need to open a file for writing using the `openFile` function. This function takes two parameters - the name of the file and the mode in which we want to open it. For our purposes, we will be using the mode "WriteMode" which allows us to write to the file. We can save the resulting file handle in a variable, which we will use to interact with the file in future operations.

```Haskell
fileHandle <- openFile "example.txt" WriteMode
```

With our file now open, we can use the `hPutStrLn` function to write a line of text to the file. For example, we can write the phrase "Hello world!" by executing the following code:

```Haskell
hPutStrLn fileHandle "Hello world!"
```

To add more lines to our file, we can simply call the `hPutStrLn` function again with the desired text. Remember to use the file handle we saved earlier, and don't forget to close the file once we are finished writing. This can be done using the `hClose` function.

```Haskell
hPutStrLn fileHandle "This is a new line!"
hClose fileHandle
```

## Deep Dive

Now that we know how to write to a text file in Haskell, let's take a deeper look at some of the concepts involved. One important thing to note is that the `openFile` function returns an `IO Handle`, which means it operates within the IO monad. This allows us to perform I/O operations in a safe and controlled manner, preventing any unexpected or unintended side effects.

When using the `openFile` function, we specified the mode in which we wanted to open the file. In addition to "WriteMode", there are other modes such as "ReadMode" and "AppendMode" which allow us to read from and append to a file, respectively. It is important to use the correct mode depending on the action we want to perform on the file.

We also used the `hPutStrLn` function to write a line of text to our file. This function takes a `Handle` as the first parameter and the text to be written as the second parameter. It automatically adds a newline character at the end of the text, making it convenient for writing multiple lines to a file.

Lastly, we used the `hClose` function to close our file. Failing to do so can result in data loss or other unforeseen issues. It is good practice to always close a file after we are finished writing to it.

## See Also

- [Haskell IO documentation](https://hackage.haskell.org/package/base/docs/System-IO-Handle.html)
- [A Gentle Introduction to Haskell IO](https://www.haskell.org/tutorial/io.html)
- [Learn You a Haskell for Great Good! Chapter 9](http://learnyouahaskell.com/input-and-output)
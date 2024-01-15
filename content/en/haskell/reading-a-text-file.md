---
title:                "Reading a text file"
html_title:           "Haskell recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading and processing text files is a common task in many programming languages, and Haskell is no exception. In fact, with its strong focus on data manipulation and functional programming, Haskell is a powerful tool for working with text files. Whether you need to parse large data sets or simply extract information from a text file, learning how to read and manipulate text files in Haskell is a valuable skill for any programmer.

## How To

To read a text file in Haskell, we will use the `readFile` function from the `System.IO` module. This function takes a file path as its argument and returns the contents of the file as a string. Let's see an example:

```Haskell
import System.IO

main = do
    contents <- readFile "file.txt"
    putStrLn contents
```

In this example, we import the `System.IO` module and use the `readFile` function to read the contents of a file called "file.txt". We then use the `putStrLn` function to print the contents of the file to the console. Running this code will output the contents of the file to the console.

But what if we want to do more than just print the contents of the file? Haskell provides us with a rich set of functions for manipulating strings, making it easy to parse and extract information from a text file. Let's see another example:

```Haskell
import System.IO
import Data.List

main = do
    contents <- readFile "file.txt"
    let lines = lines contents
    let firstLine = head lines
    let words = words firstLine
    let reversed = reverse words
    putStrLn (intercalate " " reversed)
```

In this example, we use the `lines` function to split the file contents into a list of lines, and then use the `words` function to split the first line into a list of words. We then use the `reverse` function to reverse the order of the words, and the `intercalate` function to join the words back together with a space in between. Running this code will output the first line of the file in reverse order.

## Deep Dive

Under the hood, the `readFile` function uses lazy evaluation, meaning it will only read the contents of the file as it is needed. This can be useful when dealing with large files, as it allows us to work with them without needing to load the entire file into memory.

It's also worth noting that the `readFile` function will return the contents of the file as a string, which may not always be the most efficient data type for working with text. Depending on the task at hand, it may be more beneficial to use the `ByteString` or `Text` data types, which are specifically designed for handling text data in Haskell.

## See Also

- [Haskell Documentation on IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#the-hello-world-of-functional-programming)
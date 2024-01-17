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

## What & Why?

Reading a text file means extracting data from a file that contains text. This is a common task that programmers do when working with data that needs to be processed in a structured way. Text files can contain anything from simple text to complex data sets, making them a versatile and easily accessible format for data storage.

## How to:

To read a text file in Haskell, we will be using the ```readFile``` function from the ```System.IO``` module. This function takes in a filepath, reads the contents of the file, and returns it as a string.

```Haskell
import System.IO

main = do
  file <- readFile "my_file.txt"
  putStrLn file
```

In the above example, we first import the ```System.IO``` module which contains the ```readFile``` function. Then, we use ```readFile``` to read the contents of the file "my_file.txt" and store it in the variable ```file```. Finally, we use the ```putStrLn``` function to print the contents of the file to the console.

```
This is the content of my text file.
It can contain multiple lines.
```

## Deep Dive:

In Haskell, reading a text file is a simple task thanks to the ```readFile``` function. However, this was not always the case. In earlier versions of Haskell, programmers had to use the ```openFile``` and ```hGetContents``` functions to achieve the same result. The ```readFile``` function was introduced in version 1.3 of Haskell, making it easier and more straightforward to read a text file.

There are also other ways to read a text file in Haskell, such as using the ```Data.ByteString``` module for a more efficient performance, or using the ```Data.Text``` module for handling Unicode characters. However, these methods require more advanced knowledge and are beyond the scope of this article.

## See Also:

For more information on reading text files in Haskell, check out these resources:

- [Haskell Documentation - System.IO module](https://www.haskell.org/onlinereport/io.html#sect6.2)
- [Real World Haskell - File I/O](http://book.realworldhaskell.org/read/io.html)
- [Learn You a Haskell - I/O](http://learnyouahaskell.com/input-and-output)
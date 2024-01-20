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

## What & Why?

Writing a text file in Haskell involves creating a file and filling it with textual data. Programmers use this method to store data in a format that can be easily read and modified by both humans and machines.

## How to:

To write a text file in Haskell, we use the `writeFile` function from the `System.IO` module. This function takes in two arguments: the file name and the text to be written. For example:

```Haskell
writeFile "myFile.txt" "This is the content of my file."
```
This creates a file named "myFile.txt" and writes the text "This is the content of my file." inside it. 

To write multiple lines, we can use the `unlines` function to concatenate our lines into a single string, then pass it as the second argument to `writeFile`. Here's an example:

```Haskell
let lines = ["Line 1", "Line 2", "Line 3"]
writeFile "myFile.txt" (unlines lines)
```
This will create a file named "myFile.txt" and write the contents of `lines` as separate lines in the file.

## Deep Dive:

In earlier versions of Haskell, the `writeFile` function was not available and programmers had to use the `openFile` and `hPutStr` functions to achieve the same result. However, the `writeFile` function is now the preferred method as it simplifies the code and takes care of opening and closing the file for us.

An alternative to writing text files in Haskell is using the `Data.ByteString` module, which deals with raw bytes instead of characters. This can be more efficient for handling large amounts of data.

## See Also:

- [Haskell Data.ByteString module documentation](http://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html)
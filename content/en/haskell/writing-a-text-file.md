---
title:    "Haskell recipe: Writing a text file"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are an essential part of any programming language as they allow us to store and retrieve data easily. In Haskell, writing a text file can be done in a few simple steps and can greatly enhance the functionality of your code. Plus, it's a useful and practical skill to have as a programmer.

## How To

To begin with, we need to import the necessary modules for file input and output in Haskell. This can be done by adding the following code at the top of your file:

```Haskell
import System.IO
import Data.Char
```

Next, we need to create a file handle using the `openFile` function. This function takes in three arguments: the file name, the mode (which specifies if we want to read, write, or append to the file), and the encoding. For example, to create a new file called "myFile.txt" for writing, we can use the following code:

```Haskell
file <- openFile "myFile.txt" WriteMode
```

Once we have our file handle, we can write to the file using the `hPutStrLn` function. This function takes in the file handle and the string that we want to write to the file. For example, we can write "Hello World!" to our file as follows:

```Haskell
hPutStrLn file "Hello World!"
```

After we have finished writing to the file, it is important to close the file handle using the `hClose` function. This ensures that all the changes are saved and the file is ready to be used. We can close our file as follows:

```Haskell
hClose file
```

Now, let's take a look at a complete example. In the following code, we will ask the user to enter their name and then save it to a text file called "names.txt". We will also add a new line character to the end of each input so that each name is on a separate line in the file.

```Haskell
import System.IO
import Data.Char

main = do
  file <- openFile "names.txt" WriteMode
  putStrLn "Enter your name:"
  name <- getLine
  hPutStrLn file (name ++ "\n")
  putStrLn "Enter another name:"
  name2 <- getLine
  hPutStrLn file (name2 ++ "\n")
  hClose file
  putStrLn "Names have been saved to file."
```

If we run this code and enter "John" and "Sarah" as names, our "names.txt" file will contain the following:

```
John
Sarah
```

## Deep Dive

In Haskell, there are a few other functions and techniques that can be used to write to text files. For example, we can use the `withFile` function instead of `openFile` and `hClose`. This function takes in the file name, mode, and a function to execute with the file handle. This ensures that the file handle is automatically closed after the function's execution.

We can also use the `appendFile` function to add content to an existing file without overwriting the existing text.

Additionally, we can use the `writeFile` function to create and write to a file in one step, rather than using `openFile` and `hPutStrLn` separately.

## See Also

- [Haskell documentation on file input and output](https://www.haskell.org/tutorial/inputoutput.html)
- [TutorialsPoint's guide on file handling in Haskell](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)
- [Real World Haskell chapter on file handling](http://book.realworldhaskell.org/read/io.html)
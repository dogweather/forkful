---
title:    "Haskell recipe: Reading a text file"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're new to Haskell programming or just looking to expand your knowledge, you may be wondering why you would even want to learn how to read a text file. After all, isn't that the job of a word processing program? However, being able to read and manipulate text files is an essential skill for any programmer. Whether it's for data analysis, file processing, or building a text-based application, knowing how to read text files in Haskell will open up a whole new world of possibilities.

## How To

Reading a text file in Haskell is a simple process that can be accomplished with just a few lines of code. First, we'll need to import the necessary module:

```Haskell
import System.IO
```

Next, we'll define a function that takes in a file name as an argument and returns the contents of the file as a string:

```Haskell
readFileContents :: FilePath -> IO String
readFileContents filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    hClose handle
    return contents
```

In the above code, we use the `openFile` function to open the file in read mode and assign it to the `handle` variable. Then, we use the `hGetContents` function to read the contents of the file and assign it to the `contents` variable. Finally, we use the `hClose` function to close the file and return the contents.

Now, let's test our function by reading the contents of a sample text file called "example.txt":

```Haskell
main :: IO ()
main = do
    contents <- readFileContents "example.txt"
    putStrLn contents
```

If we run this code, the output in our terminal will be the contents of our text file:

```
This is an example text file.
It contains multiple lines of text
which we will be able to read and manipulate.
```

## Deep Dive

Reading a text file may seem like a simple task, but there are some important things to keep in mind when working with text files in Haskell. The first thing to note is that when reading a file, the contents are returned as a string. This means that any manipulation or processing we want to do on the contents will need to be done using string functions and operations.

Another thing to consider is that reading a file can be dangerous if proper error handling is not implemented. For example, if we try to read a file that does not exist, our program will throw an error and crash. To prevent this, we can use the `catch` function from the `Control.Exception` module to catch any potential errors and handle them appropriately.

Finally, it's important to remember to always close the file after reading its contents. If we leave a file open, it can lead to potential memory leaks and affect the performance of our program.

## See Also

- [Haskell I/O documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Learn You a Haskell for Great Good! - I/O](http://learnyouahaskell.com/input-and-output)
- [Haskell Crash Course - File I/O](https://www.codementor.io/@bjarkis/formatted-text-output-in-haskell-via-fprintf-n9q4fwnwz)

Now that you know how to read a text file in Haskell, you can start exploring all the different ways you can use this skill in your coding projects. Happy coding!
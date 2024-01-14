---
title:                "Haskell recipe: Reading a text file"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a common way to store and share simple data. Reading text files in Haskell allows us to access this data and use it in our programs. Whether you need to process user input, read in data from a file, or analyze a log file, knowing how to read text files in Haskell is a useful skill to have.

## How To

Reading a text file in Haskell is a straightforward process. First, we need to import the necessary module, `System.IO`, which contains the `readFile` function. This function takes the file path as its argument and returns a *lazy* string, which we can then process as needed.

Let's look at an example where we want to read a text file named `input.txt` and print its contents to the console:

```Haskell
import System.IO

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn file
```

In the above code, we use the `readFile` function to read the contents of the file into the `file` variable. After that, we use the `putStrLn` function to print the contents of the file to the console. By using a *lazy* string, the entire file is not read into memory at once, allowing us to process large files without any performance issues.

We can also manipulate the contents of the file before printing it out. For example, if we wanted to count the number of words in the file, we could modify our code as follows:

```Haskell
import System.IO

main :: IO ()
main = do
    file <- readFile "input.txt"
    let wordCount = length $ words file
    putStrLn $ "The file contains " ++ show wordCount ++ " words."
```

In this code, we use the `length` and `words` functions to count the number of words in the file. Using the `show` function, we can convert the result into a string and print it out along with a custom message.

## Deep Dive

Behind the scenes, the `readFile` function in the `System.IO` module uses the `openFile` function from the same module to open the file and create a handle. This handle is used to read in the file's contents, which are then returned as a string.

It's worth noting that reading in a file using this method doesn't guarantee that the file will be closed after it's been read. It's advisable to use the `withFile` function, also in the `System.IO` module, which ensures that the file is closed after it's been used.

## See Also

- [Haskell documentation on `System.IO` module](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial on reading and writing files in Haskell](https://www.tutorialspoint.com/haskell/haskell_file_io.htm)
- [Haskell Wiki page on file I/O](https://wiki.haskell.org/File_manipulation)
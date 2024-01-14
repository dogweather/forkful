---
title:    "Haskell recipe: Writing a text file"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why Writing a Text File in Haskell?

There are many reasons why someone might want to write a text file in Haskell. Text files are a common way to store data, and being able to write them using Haskell can provide a more efficient and elegant solution than other languages.

## How To Write a Text File in Haskell?

To write a text file in Haskell, we will use the `writeFile` function from the `System.IO` library. Let's take a look at a simple example:

```Haskell
import System.IO

main = do
    let fileName = "sample.txt"
    let content = "This is a sample text file written using Haskell."
    writeFile fileName content
```

In the above code, we first import the `System.IO` library, which contains functions for file input/output operations. Then, we use the `writeFile` function, which takes two parameters - the file name and the content to be written. Finally, we use the `main` function to run our code.

Now, let's run this code and see what happens. After running the code, we will see that a new file named "sample.txt" has been created in the same directory as our Haskell file.

When we open the "sample.txt" file, we can see that it contains the text "This is a sample text file written using Haskell." This is the content that we passed to the `writeFile` function. That's it, we have successfully written a text file using Haskell!

## Deep Dive into Writing a Text File in Haskell

The `writeFile` function uses lazy I/O, which means that the file will only be written when it is actually needed. This can be useful for handling large amounts of data, as it allows for more efficient memory usage.

One important thing to note is that the `writeFile` function will overwrite any existing file with the same name. If we want to append to an existing file instead of overwriting it, we can use the `appendFile` function.

```Haskell
appendFile :: FilePath -> String -> IO ()
```

The `FilePath` parameter is the name of the file we want to append to, and the `String` parameter is the content we want to add.

Another useful function for writing text files in Haskell is the `hPutStrLn` function. This function takes a file handle and a `String` parameter, and writes the String to the file followed by a newline character.

```Haskell
hPutStrLn :: Handle -> String -> IO ()
```

Using `hPutStrLn` instead of `writeFile` can be useful when we want to write multiple lines of text to a file.

## See Also

- [Haskell Documentation - System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- [Learn You a Haskell for Great Good! - File Input and Output](http://learnyouahaskell.com/input-and-output#files)
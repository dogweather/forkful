---
title:    "Haskell recipe: Creating a temporary file"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Temporary files are essential in many programming tasks, especially in Haskell, where functional programming and immutable data structures are used. They provide a way to store temporary data and handle it in a simple and efficient manner.

## How To
To create a temporary file in Haskell, we can use the `withSystemTempFile` function from the `System.IO.Temp` module. This function takes two arguments: a file prefix and a function that will handle the created temporary file. Let's take a look at a simple example:

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

main = withSystemTempFile "myTempFile.txt" $ \tempFilePath tempFileHandle -> do
    hPutStrLn tempFileHandle "Hello World!"
    hClose tempFileHandle
    putStrLn $ "Created temporary file at: " ++ tempFilePath
```

In this example, we import the necessary modules and use the `withSystemTempFile` function. We pass in a file prefix "myTempFile.txt" and a function that takes two arguments: the temporary file path and file handle. We use the file handle to write "Hello World!" to the temporary file and then close it. Finally, we print out the temporary file path to the console.

Running this code will output the following:
```
Created temporary file at: /var/folders/7p/92bhzrbj0sg6g7x9435_tjtjz8fyzg/T/myTempFile.txt
```
The temporary file is automatically deleted once the program ends or the file handle is closed.

## Deep Dive
Under the hood, the `withSystemTempFile` function uses `openTempFile` from the `System.IO` module to create the temporary file. However, `withSystemTempFile` ensures that the file is deleted after the function has finished running, even in cases of an exception.

We can also specify a directory to create the temporary file in, instead of using the system's default location. The `withSystemTempDirectory` function from the same module works similarly to `withSystemTempFile`, except it takes a directory prefix instead of a file prefix.

## See Also
- [Haskell documentation on System.IO.Temp module](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Temp.html)
- [Official Haskell website](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
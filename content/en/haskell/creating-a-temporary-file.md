---
title:    "Haskell recipe: Creating a temporary file"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##
Why Create a Temporary File in Haskell?

Temporary files are a necessary part of any programming language, and Haskell is no exception. Creating temporary files can be useful for a variety of reasons, such as storing data that will only be needed for a short period of time, or for testing purposes. In this blog post, we'll discuss why you may need to create a temporary file in Haskell, and how to do so efficiently.

## 
How To Create a Temporary File in Haskell

Creating a temporary file in Haskell is a simple process, thanks to the standard library function `withSystemTempFile`. This function takes two arguments: a file prefix and a function that takes a `FilePath` as its argument. Here's an example of how to use it:

```Haskell
import System.IO.Temp (withSystemTempFile)

main = withSystemTempFile "mytempfile" $ \filePath handle -> do
    hPutStrLn handle "This is a temporary file"
    putStrLn $ "Temp file path: " ++ filePath
```

Running this code will create a temporary file with the given prefix (in this case, "mytempfile") in the system's temporary directory (usually `/tmp` on Unix systems). The file will be automatically deleted after the function finishes executing.

The `withSystemTempFile` function also returns the `FilePath` of the created temporary file, which can be useful for performing further actions with the file. For example, you can read from and write to the file using the handle that is passed to the function.

The output of the above code will be:

```
Temp file path: /tmp/mytempfile123.tmp
```

## 
Deep Dive into Creating Temporary Files in Haskell

Under the hood, the `withSystemTempFile` function is using the `openTempFile` function from the `System.IO` module. This function allows you to specify the directory where you want the temporary file to be created, as well as the file mode and file prefix. However, `withSystemTempFile` takes care of all of these details for us, making it a convenient and simple way to create temporary files in Haskell.

It's also worth noting that the `withSystemTempFile` function is platform-agnostic, meaning it will work on both Windows and Unix systems without any modifications to the code.

## 
See Also

- [Haskell Docs on temporary files](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Real World Haskell - Creating and Manipulating Files](http://book.realworldhaskell.org/read/io.html#id624117)
- [Haskell Wiki - Working with Temporary Files](https://wiki.haskell.org/Working_with_temporary_files)
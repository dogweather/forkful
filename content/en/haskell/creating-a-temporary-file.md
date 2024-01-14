---
title:                "Haskell recipe: Creating a temporary file"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating a temporary file is a useful tool in any programmer's arsenal. Temporary files are files that are created and used only for a short period of time, typically for the purpose of storing data or information that is needed temporarily and will be deleted when no longer needed. In Haskell, temporary files can be especially helpful when dealing with large amounts of data or when testing code that requires file input.

## How To

To create a temporary file in Haskell, we will use the System.IO.Temp module. First, we will need to import this module in our code:

```Haskell
import System.IO.Temp
```

Next, we will use the `withSystemTempFile` function to create a temporary file and perform any actions on it within the scope of the function. The function takes two parameters: a file pattern and a function that will be applied to the temporary file. The file pattern is a String that specifies the naming pattern of the temporary file. It can include any combination of characters, but it must include the string "XXXXX", which will be replaced with a random sequence of characters to ensure it is a unique file.

```Haskell
withSystemTempFile "myTempFile-XXXXX.txt" $ \path handle -> do
    -- actions to perform on the temporary file
```

Within the scope of the function, the temporary file will be created and opened for reading and writing. The `path` variable will contain the path to the temporary file, and the `handle` variable will be a handle to the file. When the function completes, the temporary file will automatically be deleted.

We can now perform any necessary actions on the file using standard file I/O operations. For example, we can write to the file like this:

```Haskell
hPutStrLn handle "This is a temporary file."
```

And we can read from the file like this:

```Haskell
contents <- hGetContents handle
putStrLn contents -- prints the contents of the temporary file
```

## Deep Dive

Behind the scenes, the `withSystemTempFile` function uses the `openTempFile` and `hClose` functions from the `System.IO` module to create and close the temporary file. The `path` and `handle` variables are just the return values of these functions, making it easy for us to access and use them within our code. Additionally, the `withSystemTempFile` function takes care of properly deleting the temporary file even in the case of an error occurring within the function.

It is important to note that the temporary file is created in the system's temporary directory, which may vary depending on the operating system. In Windows, this is typically the path specified by the `TEMP` or `TMP` environment variables. In Unix-based systems, this is often the `/tmp` directory.

## See Also

- [Haskell documentation for System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3.0.0/docs/System-IO-Temp.html)
- [Haskell documentation for System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell documentation for file I/O](https://en.wikibooks.org/wiki/Haskell/File_IO)
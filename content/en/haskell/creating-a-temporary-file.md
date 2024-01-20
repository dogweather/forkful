---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file means generating a unique file for short-term storage. Programmers do it to handle large data chunks without exhausting memory, store session-specific info, or provide buffers for data exchange between processes.

## How to:

The `System.IO.Temp` module in Haskell provides functions to create temporary files. Here's how you can create one:

```Haskell
import System.IO.Temp (withSystemTempFile)

main = withSystemTempFile "temp.txt" $ \tempFilePath tempFileHandle -> do
    putStrLn $ "A temporary file has been created at: " ++ tempFilePath
```

Running this code will print a message with the temporary file's path:

```
A temporary file has been created at: /tmp/temp.txt12345
```

## Deep Dive

Temporary file creation isn't new, dating back to the early days of single-user systems. It was then repurposed for multi-user systems.

There are alternatives, including in-memory data storage structures like `Data.Text` or `Data.ByteString.Lazy`. Yet, these aren't always viable when handling enormous data volumes or when persistence across sessions is required.

While implementing, note that these temporary files have permission defaults to be readable and writable only by the file creator. It's also important to know that Haskell's garbage collector removes these files when they're no longer in use by the program.

## See Also

To learn more, check out these sources:

- [Haskell's I/O Inside: Temporary Files](https://wiki.haskell.org/Tutorials/Programming_Haskell/IO_inside#Temporary_files)
- [Haskell System.IO.Temp Library Documentation](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
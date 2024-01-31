---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file means storing data like strings into a file on your disk. Programmers do it for logging, saving configurations, or plain data persistence.

## How to:
You can write a text file in C# using `File.WriteAllText`, `File.AppendAllText`, or a `StreamWriter`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Write text to a new file
        File.WriteAllText("log.txt", "Hello, file!");

        // Append text to the existing file
        File.AppendAllText("log.txt", "\nLet's add another line.");

        // Use StreamWriter to write to a file
        using (StreamWriter writer = new StreamWriter("log.txt", true))
        {
            writer.WriteLine("Another line with StreamWriter.");
        }
    }
}
```

Sample output in `log.txt`:
```
Hello, file!
Let's add another line.
Another line with StreamWriter.
```

## Deep Dive
Historically, file I/O in C# has evolved from basic `FileStream` operations to abstractions like `StreamWriter`. Alternatives include using `System.IO.FileStream` for more control or asynchronous methods like `WriteAllTextAsync` for efficiency. Under the hood, `StreamWriter` uses a buffer to optimize writing operations.

## See Also
For related reading and in-depth tutorials:
- [MSDN Documentation on File I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [MSDN StreamWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [Tutorial on Asynchronous File I/O in C#](https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o)

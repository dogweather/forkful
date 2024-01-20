---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file involves retrieving content from a file that's formatted as human-readable text. Programmers do this to consume data, like configuration details, or process content for applications such as data analysis or machine learning.

## How To:

In C#, we'll use the `StreamReader` class from the `System.IO` namespace to read a text file. Here's a simple example:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        using (StreamReader reader = new StreamReader(@"C:\yourfile.txt"))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

In this example, lines from "yourfile.txt" are read and displayed in the console one by one.

For more simplified file reading tasks, you could use `File.ReadAllLines()` or `File.ReadAllText()`:

```C#
string[] lines = File.ReadAllLines(@"C:\yourfile.txt");
```

```C#
string text = File.ReadAllText(@"C:\yourfile.txt");
```

These methods load the entire file content into memory, so use them carefully with large files.

## Deep Dive

Historically, file reading in C# has evolved. Initially, one had to manually manage file streams and transform the byte data to a human-readable format. With the introduction of the `StreamReader` class and similar utilities, the process is much simpler.

Alternatives to `StreamReader` include `FileStream` and `BufferedStream`, which offer a lower-level access to file data and more control over the reading process. These tend to be more complex to work with and are generally used when you need more control over file access or when working with non-text data.

When it comes to implementation details, remember that all forms of file reading involve working with I/O operations. These can cause exceptions due to issues like file access permissions or non-existent paths, and should be handled appropriately. In C#, this is often done with the `try-catch` statement. Additionally, don't forget to close streams after reading to free up system resources.

## See Also:

For more details, consider these resources:
- Microsoft's official documentation on File I/O: [https://docs.microsoft.com/en-us/dotnet/standard/io/](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- Various methods to read a text file: [https://stackoverflow.com/questions/5282999/reading-text-file-in-c-sharp](https://stackoverflow.com/questions/5282999/reading-text-file-in-c-sharp)
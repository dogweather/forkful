---
title:                "C# recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered the need to read a large text file in your C# program? Perhaps you need to extract important information from a log file or parse a CSV file for data analysis. Reading a text file is a common task in programming, and knowing how to do it efficiently can save you time and frustration.

## How To

Reading a text file in C# is a simple process that can be accomplished using the built-in File class. First, we need to import the System.IO namespace:

```C#
using System.IO;
```

Next, we can use the static method ReadAllText() from the File class to read the contents of a text file into a string variable:

```C#
string text = File.ReadAllText("sample.txt");
```

This will read the entire file and store its contents in the "text" variable. However, if you have a large file, it may not be efficient to read the entire file at once. In that case, we can use the StreamReader class to read the file line by line:

```C#
using (StreamReader sr = File.OpenText("sample.txt"))
{
    string line;
    while ((line = sr.ReadLine()) != null)
    {
        // Do something with each line
        Console.WriteLine(line);
    }
}
```

Using the "using" statement ensures that the file is closed correctly after we finish reading it.

## Deep Dive

There are a few things to keep in mind when reading a text file in C#. By default, the ReadAllText() method will use the UTF-8 encoding, but you can specify a different encoding if needed. You can also use the StreamReader class to specify the encoding and other options.

Additionally, when reading a large file, it is better to use the ReadLine() method instead of ReadAllText(), as it will not consume as much memory.

It is also important to handle exceptions when reading a file, as it may not always be available or in the correct format. You can use try-catch blocks to handle these potential errors.

## See Also

- [File.ReadAllText Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)
- [StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [Encoding Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.encoding)
- [Exceptions in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)
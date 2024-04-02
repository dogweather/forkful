---
date: 2024-01-20 17:53:50.228158-07:00
description: "Reading a text file is grabbing data from a file that contains text.\
  \ Programmers do this to load configurations, read data, or fetch resources that\
  \ are\u2026"
lastmod: '2024-03-13T22:45:00.104938-06:00'
model: gpt-4-1106-preview
summary: "Reading a text file is grabbing data from a file that contains text. Programmers\
  \ do this to load configurations, read data, or fetch resources that are\u2026"
title: Reading a text file
weight: 22
---

## What & Why?
Reading a text file is grabbing data from a file that contains text. Programmers do this to load configurations, read data, or fetch resources that are too bulky or inappropriate to hard-code.

## How to:
Let's get straight to it. Here's how you read from a file in C# using `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        
        // Reading all text
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // Reading lines into an array
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // Reading with a StreamReader
        using (StreamReader reader = new StreamReader(filePath))
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

Sample Output:

```
Hello, this is a text file.
It has multiple lines.
Each line will be read separately.
```

## Deep Dive
Reading a text file seems simple enough, right? But there’s a bit of history and some nuances worth knowing.

Back in the day, text files were often the primary way to store data before databases were commonly used. Programmers had to manage file access, format data correctly, and handle errors. C# has evolved a lot since then. Now, `System.IO` is your go-to namespace for file operations.

You've got options:

- `File.ReadAllText` reads the whole shebang in one go—great for smaller files.
- `File.ReadAllLines` gives you each line as an array element—handy for processing lines.
- `StreamReader` reads line-by-line, which is more memory efficient for big files.

Each method locks the file while it's in use. This is important if other processes might be trying to access the file.

Remember, always handle exceptions such as `FileNotFoundException` or `IOException` when dealing with files. You don’t want your app crashing unexpectedly.

## See Also
Have more questions or looking to expand your knowledge? Check out these links:

- [MSDN Documentation on File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [MSDN Documentation on StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Tutorial on exception handling](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)

---
title:                "C# recipe: Reading a text file"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why 
Reading and parsing text files is a critical skill for any programmer. Text files are a common way of storing, sharing, and analyzing data. Being able to read and manipulate text files in your code can greatly enhance the functionality and usefulness of your programs.

## How To 
Reading a text file in C# is a straightforward process. First, we need to create a new StreamReader object and specify the path of the text file to be read.

```C#
StreamReader reader = new StreamReader("example.txt");
```

Next, we can use the `ReadLine()` method to read the next line from the file and store it in a string variable. This method will keep reading the file until it reaches the end.

```C#
string line = reader.ReadLine();
```

To read the entire file, we can use a while loop that checks if the line is not null. Inside the loop, we can perform any necessary operations on the line of text.

```C#
while (line != null)
{
    // operations on line
    line = reader.ReadLine();
}
```

Finally, we need to close the StreamReader object to release any resources associated with it.

```C#
reader.Close();
```

Let's look at a simple example where we read and print out the contents of a text file:

```C#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        StreamReader reader = new StreamReader("example.txt");
        string line = reader.ReadLine();

        while (line != null)
        {
            Console.WriteLine(line);
            line = reader.ReadLine();
        }

        reader.Close();
    }
}
```

## Deep Dive 
There are various other techniques and methods that can be used to read text files in C#. You can use the `ReadToEnd()` method to read the entire file as a string, or the `Read()` method to read a specific number of characters. Additionally, you can use the `FileStream` class to read binary data from a file.

It's also important to handle potential errors when reading a text file, such as file not found or permission issues. You can use try-catch blocks to catch any exceptions and handle them accordingly.

## See Also 
- [C# File Handling](https://www.geeksforgeeks.org/file-handling-c-sharp/)
- [C# String Operations](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [C# Exceptions](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)
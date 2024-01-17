---
title:                "Reading a text file"
html_title:           "C# recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file involves opening and reading the contents of a file which contains human-readable text. Programmers do this in order to access and manipulate the data contained within the file, which can include documents, configurations, and settings for a program.

## How to:

To read a text file in C#, we first need to import the System.IO namespace. Then, we use the StreamReader class to open the file and read its contents line by line. Here is an example of how to do this:

```C#
using System.IO;

// Open the file using a StreamReader
StreamReader reader = new StreamReader("myfile.txt");

// Read the contents of the file line by line
string line;
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}

// Close the file
reader.Close();
```

The above code will print out the contents of the text file "myfile.txt" to the console. 

## Deep Dive

Reading text files has been a core feature of programming languages since their inception. Prior to the widespread use of graphical user interfaces, text files were the primary means of storing and exchanging data. Today, while there are alternatives such as databases and XML files, text files remain a popular choice for storing and sharing data due to their simplicity and portability.

In C#, there are multiple ways to read a text file. While the above example uses the StreamReader class, there is also the File class, which provides static methods for working with files. Additionally, the TextFieldParser class can be used for parsing and reading CSV and other delimited text files.

When reading a text file, it is important to handle potential errors that may occur. For example, the file may not exist or the program may not have permission to access it. In these cases, the code should include exception handling to gracefully handle these situations.

## See Also

- [C# Guide: Reading Text Files](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-line-by-line)
- [File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [TextFieldParser Class](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.fileio.textfieldparser?view=netcore-3.1)
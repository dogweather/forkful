---
title:                "Writing a text file"
html_title:           "C# recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're familiar with C# programming, you probably know that sometimes you need to save and read data from external files. One of the most common types of files to work with is a text file, which stores data in a human-readable format. In this article, we'll discuss how to write a text file in C# and why it's necessary.

## How To

To start, we need to import the `System.IO` namespace in order to use file-related classes and methods. Then, we'll create a new `StreamWriter` object and specify the file path and name we want to write to. Next, we can use the `WriteLine()` method to add text to the file, and the `Close()` method to save and close the file.

```C#
using System.IO;

StreamWriter writer = new StreamWriter("myFile.txt");

writer.WriteLine("Hello World!");
writer.WriteLine("This is a sample text file.");

writer.Close();
```
The above code will create a text file named `myFile.txt` in the same directory as your program. If the file already exists, its contents will be overwritten.

## Deep Dive

If you want to add multiple lines of text to your file without using repeated `WriteLine()` statements, you can use the `Write()` method instead. This will write the text without adding a new line character at the end. You can also use the `Write()` method to write data of different types, such as numbers or boolean values.

Additionally, you can use the `AppendText()` method of the `File` class to add new lines of text to an existing file without overwriting its contents. This can be useful if you want to constantly update a log file or keep track of changes in a data file.

## See Also

- [C# StreamWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=net-5.0)
- [C# File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)
- [C# File I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
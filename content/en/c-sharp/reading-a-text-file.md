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

## Why

Reading a text file is a fundamental task in programming. Whether you need to process user input, parse a configuration file, or analyze large amounts of data, understanding how to read a text file using C# can greatly enhance your coding skills and make your programs more robust.

## How To

To read a text file in C#, you will need to use the `System.IO` namespace. This namespace contains the necessary classes and methods for handling files. Specifically, the `File` class provides static methods for reading and writing files, and the `StreamReader` class allows you to read from a file stream.

To begin, you'll need to create a `StreamReader` object and pass in the path to your text file as a parameter. In the following example, we'll read a text file named "sample.txt" from the desktop:

```C#
using System;
using System.IO;

StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt");
```

Next, you can use the `ReadLine()` method on the `StreamReader` object to read one line of the text file at a time. This method returns a string, so you can store it in a variable and use it in your code. The `ReadLine()` method will return `null` when it reaches the end of the file, so be sure to include a condition to check for this.

```C#
using System;
using System.IO;

StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt");

string line;
while ((line = reader.ReadLine()) != null)
{
    // Do something with each line of the text file
    Console.WriteLine(line);
}
```

You can also use the `ReadToEnd()` method to read the entire content of the file as a single string:

```C#
using System;
using System.IO;

StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt");

string content = reader.ReadToEnd();
```

Once you've finished reading the file, it's important to close the `StreamReader` object to release any resources it was using. You can do this by calling the `Close()` method on the object.

```C#
using System;
using System.IO;

StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt");

// Read file content

reader.Close(); // Make sure to close the StreamReader object
```

## Deep Dive

There are several options available for reading text files in C#, including the `File` and `StreamReader` classes that we covered above. It's important to consider factors such as performance, error handling, and memory management when choosing which option to use.

One useful feature of the `StreamReader` class is the ability to specify the character encoding of the file you're reading. This is particularly helpful when working with international text files that may use different character sets. To specify an encoding, you can pass in a `System.Text.Encoding` object as a second parameter when creating the `StreamReader` object.

```C#
using System;
using System.IO;
using System.Text;

StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt", Encoding.UTF8);
```

Another important aspect to consider when reading text files is error handling. It's important to handle any exceptions that may occur, such as a file not being found or being unable to open the file for some reason. You can use a `try...catch` block to handle these exceptions and prevent your program from crashing.

```C#
using System;
using System.IO;
using System.Text;

try
{
    StreamReader reader = new StreamReader(@"C:\Users\Username\Desktop\sample.txt", Encoding.UTF8);

    // Read file content

    reader.Close();
}
catch (Exception ex)
{
    Console.WriteLine(ex.Message);
}
```

## See Also

- Microsoft Docs - Read Text from a File in C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file
- C# Text File Handling: https://www.tutorialspoint.com/csharp/csharp_text_file_handling.htm
- C# File class: https://www.geeksforgeeks.org/file-class-in-c-sharp/
---
title:                "C# recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, text files are still a crucial aspect of programming. They allow us to store data in a human-readable format that can easily be accessed and manipulated by different programs. Whether you're writing a simple script or a complex application, text files are a versatile tool that every programmer should have in their arsenal.

## How To

Writing a text file in C# is a straightforward process. First, we need to import the necessary libraries:

```C#
using System.IO; // for file operations
using System.Text; // for text encoding
```

Next, we can use the `StreamWriter` class to create a new text file and write to it. We need to provide the file path where we want to create the file, the text encoding format, and whether we want to append to an existing file or create a new one.

```C#
// create a new text file and write to it
using (StreamWriter writer = new StreamWriter("C:/example.txt", false, Encoding.UTF8)) 
{
    // write a string
    writer.WriteLine("Hello world!");
    
    // write a formatted string
    string name = "John";
    int age = 28;
    writer.WriteLine("My name is {0} and I am {1} years old.", name, age);
}
```

We can also use the `File` class to write to a text file instead of using a `StreamWriter`:

```C#
// write to a text file using the File class
string[] lines = {"This is line 1", "This is line 2", "This is line 3"};
File.WriteAllLines("C:/example2.txt", lines);
```

Our code above will create two text files - "example.txt" with two lines of text and "example2.txt" with three lines of text. 

## Deep Dive

Apart from writing plain text to a file, we can also write more complex data types such as arrays or objects. We can use the `ToString()` method to convert these data types into strings and then write them to a text file.

```C#
// write an array to a text file
string[] fruits = {"apple", "orange", "banana"};
File.WriteAllText("C:/fruits.txt", fruits.ToString());

// write an object to a text file
Person person = new Person("Jane", 30);
File.WriteAllText("C:/person.txt", person.ToString());
```

Another useful feature of writing text files is the ability to use different encodings. In the second parameter of our `StreamWriter` above, we used `Encoding.UTF8` to specify the text encoding. Other options include `Encoding.ASCII`, `Encoding.Unicode`, and `Encoding.Default`, which uses the default system encoding.

## See Also

To learn more about working with text files in C#, check out these helpful resources:

- [Working with Text Files in C#](https://www.c-sharpcorner.com/article/working-with-text-file-in-C-Sharp/)
- [Writing Text Files in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [C# File Handling](https://www.tutorialspoint.com/csharp/csharp_file_handling.htm)
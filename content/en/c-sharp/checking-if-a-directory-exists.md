---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a common task that programmers do to determine whether a specified directory exists on a computer's file system. This process is important for various reasons, including error handling, file management, and ensuring proper program functionality.

## How to:

To check if a directory exists in C#, we can use the `Directory.Exists()` method. This method accepts a string parameter representing the directory path and returns a boolean value indicating whether the directory exists. Here's an example of how we can use this method in our code:

```C#
// Check if the "Documents" directory exists
bool doesExist = Directory.Exists(@"C:\Users\John\Documents");

// Print the result to the console
Console.WriteLine("Does the \"Documents\" directory exist? " + doesExist);
```

This code will check if the "Documents" directory exists in the specified path and print the result to the console as either `True` or `False`.

## Deep Dive:

### Historical Context:

In earlier versions of C#, checking if a directory exists required using the `FileInfo` class or `DirectoryInfo` class. The `Exists()` method was later added to the `Directory` class in .NET Framework 1.1, making it much simpler to perform this task.

### Alternatives:

Apart from using the `Directory.Exists()` method, there are other ways to check if a directory exists in C#. One alternative is to use the `DirectoryInfo` class, which provides various methods and properties for interacting with directories.

### Implementation Details:

Internally, the `Directory.Exists()` method uses the Win32 API `GetFileAttributes()` function to check the attributes of the specified file or directory. It then checks for the `FILE_ATTRIBUTE_DIRECTORY` attribute to determine if the given path is a directory or not.

## See Also:

To learn more about checking if directories exist in C#, you can refer to the following resources:

- [Microsoft Docs: Directory.Exists() Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Microsoft Docs: DirectoryInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- [Stack Overflow: Checking if Directory Exists in C#](https://stackoverflow.com/questions/3253052)
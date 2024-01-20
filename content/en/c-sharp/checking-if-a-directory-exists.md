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

# Checking If a Directory Exists in C#

## What & Why?

Checking if a directory exists is about verifying the presence of a folder in a specific location within the file system, from your C# code. Programmers frequently do this to avoid runtime errors and exceptions when attempting to access or manipulate directories that might, or might not, exist.

## How to:

In C#, we use the `Directory.Exists` method to check if a directory exists. Here's a simple example:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\SomeDirectory";

        if (Directory.Exists(path))
        {
            System.Console.WriteLine("Exists");
        }
        else
        {
            System.Console.WriteLine("Does not exist");
        }
    }
}
```

If `C:\SomeDirectory` exists, this program prints "Exists". Otherwise, it prints "Does not exist".

## Deep Dive

The `Directory.Exists` method has been part of the .NET framework since its inception, and it's probably the most commonly-used method to check for a directory's existence due to its simplicity and efficiency.

Of course, there are alternative ways to ascertain the existence of a directory. One could attempt to open the directory with `Directory.GetDirectories`, which throws an exception if the directory doesn't exist. However, this is less efficient and considered bad practice since exceptions should be reserved for truly exceptional circumstances.

Internally, `Directory.Exists` is a wrapper around the Win32 function `GetFileAttributes`, which has been part of Windows since the earliest days of the operating system. Despite its age, this function remains the most efficient way to determine if a directory exists.

## See Also
Please refer to the following resources for more in-depth information:
- Microsoft Official Documentation - Directory.Exists Method: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists
- Stack Overflow: https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory
- DotNetPerls - Directory.Exists: https://www.dotnetperls.com/directory-exists
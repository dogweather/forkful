---
title:    "C# recipe: Checking if a directory exists"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to check if a directory exists in your C# program? Well, this simple task may seem trivial but it is an important step in ensuring the proper functionality of your code. By checking if a directory exists, you can avoid any errors and unexpected behaviors in your program. In this blog post, we will explore how to check if a directory exists in C# and why it is necessary.

## How To

To check if a directory exists in C#, we can use the ```Directory.Exists()``` method from the ```System.IO``` namespace. This method takes in the path of the directory as a parameter and returns a boolean value indicating whether the directory exists or not.

Let's take a look at a simple code example:

```C#
using System;
using System.IO;

namespace DirectoryCheckExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Path of the directory to be checked
            string directoryPath = @"C:\Users\Username\Documents";

            // Checking if the directory exists
            if (Directory.Exists(directoryPath))
            {
                Console.WriteLine("The directory exists.");
            }
            else
            {
                Console.WriteLine("The directory does not exist.");
            }
        }
    }
}
```

#### Output:
```
The directory exists.
```

In this example, we first declare the path of the directory we want to check. Then, we use an if-else statement to check if the directory exists using the ```Directory.Exists()``` method. If it exists, we print a message stating so, and if not, we print a message indicating that it does not exist.

It is also important to note that the ```Directory.Exists()``` method only checks for the existence of a directory, not its contents. If you need to check if a specific file exists within a directory, you can use the ```File.Exists()``` method in a similar way.

## Deep Dive

The ```Directory.Exists()``` method internally uses the Win32 API method ```GetFileAttributesEx()``` to check if a directory exists. It returns true if the directory exists and does not return any exceptions if it does not (instead of throwing an exception like the ```DirectoryInfo.Exists()``` method). This makes it a more efficient and cleaner way of checking for directory existence in C#.

Another important thing to note is that the ```Directory.Exists()``` method does not require any permissions to be set explicitly for the specified directory. This is because it uses the security context of the current user to perform the check.

## See Also

- [Directory.Exists Method (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [File.Exists Method (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.exists)
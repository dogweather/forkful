---
title:                "Checking if a directory exists"
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking for a directory's existence in C# lets you confirm if a particular folder is available on the file system. Programmers do it to avoid errors like trying to read from or write to a non-existent directory, which would cause their programs to crash or behave unpredictably.

## How to:
Here's how you can check if a directory exists using the `System.IO` namespace:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\exampleFolder";

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
```
Sample Output:
```
The directory exists.
```
Or, if the directory is not found:
```
The directory does not exist.
```

## Deep Dive
The `System.IO` namespace has been around since the early days of .NET, providing tools for file and directory operations. When checking the existence of a directory, under the hood, it taps into the system API to query the file system -- an operation typically cheap in terms of system resources.

There's also the `DirectoryInfo` class, offering an object-oriented way to interact with directories. It can be slower for just checking existence since it creates an object with more data than just the existence state, but it's handy for more complex operations.

```C#
DirectoryInfo dirInfo = new DirectoryInfo(directoryPath);
if (dirInfo.Exists)
{
    // Do something with the directory.
}
```

Before `System.IO`, developers might've used platform-specific APIs or shelled out to command-line utilities to check if a directory exists, both of which are messy and fraught with issues. `System.IO` abstracted this away nicely.

It's crucial to note that existence checks can be subject to race conditions. Just because a directory exists when you check doesn't guarantee it'll exist a moment later when you try to use it, due to potential changes by other processes or users.

## See Also
- [MSDN Documentation on System.IO.Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [MSDN Documentation on System.IO.DirectoryInfo](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- [StackOverflow discussion on checking directory existence](https://stackoverflow.com/questions/1410127/c-sharp-test-if-user-has-write-access-to-a-folder)
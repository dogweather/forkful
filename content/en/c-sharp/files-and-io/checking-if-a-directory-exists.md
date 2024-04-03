---
date: 2024-02-03 19:02:42.891176-07:00
description: 'How to: #.'
lastmod: '2024-03-13T22:45:00.102348-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Checking if a directory exists
weight: 20
---

## How to:


### Using System.IO
C# provides the `System.IO` namespace which contains the `Directory` class, offering a direct way to check for a directory's existence through the `Exists` method. 

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Check if the directory exists
        bool directoryExists = Directory.Exists(directoryPath);

        // Print the result
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Sample Output:**

```
Directory exists: False
```

In case the directory does exist at the path `C:\ExampleDirectory`, the output will be `True`.

### Using System.IO.Abstractions for unit testing
When it comes to making your code unit testable, especially when it interacts with the file system, the `System.IO.Abstractions` package is a popular choice. It allows you to abstract and mock file system operations in your tests. Here's how you could check for a directory's existence using this approach:

First, ensure you have installed the package:

```
Install-Package System.IO.Abstractions
```

Then, you can inject an `IFileSystem` into your class and use it to check if a directory exists, which allows for easier unit testing.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Sample Output:**

```
Directory exists: False
```

This approach decouples your application logic from direct file system access, making your code more modular, testable, and maintainable.

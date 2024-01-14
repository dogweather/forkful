---
title:    "C# recipe: Checking if a directory exists"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
When working with file and directory operations in C#, it is important to check if a directory exists before attempting to perform any actions on it. This prevents errors and allows for better handling of files and directories in your code.

## How To
To check if a directory exists in C#, we can use the `Directory.Exists` method. This method takes in a string parameter representing the directory path and returns a boolean value indicating whether the directory exists or not.

```
C# code block:
string directoryPath = @"C:\Users\Username\Documents\MyDirectory";
bool exists = Directory.Exists(directoryPath);
Console.WriteLine("Directory exists: " + exists);

Output:
Directory exists: True
```

We can also perform additional checks on the returned boolean value to handle different cases. For example, if the directory does not exist, we can create it using the `Directory.CreateDirectory` method.

```
C# code block:
if (!exists)
{
    Directory.CreateDirectory(directoryPath);
    Console.WriteLine("Directory created.");
}
```

## Deep Dive
The `Directory.Exists` method uses the underlying Windows API function `GetFileAttributes` to check if the directory exists. It returns `INVALID_FILE_ATTRIBUTES` if the directory does not exist, or if the user does not have the required permissions to access the directory.

It is also important to note that the `Directory.Exists` method only checks the existence of the directory and does not differentiate between a file with the same name as the directory. So if a file with the same name as the directory exists, the method will still return `true`.

## See Also
- [Directory.Exists method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Directory.CreateDirectory method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.createdirectory)
- [Comparison of C# and Windows API file and directory operations](https://docs.microsoft.com/en-us/dotnet/standard/io/file-path-formats#comparison-of-file-paths-with-windows-apis)

Checking if a directory exists is a simple yet important step in C# programming. It allows for better control and handling of file and directory operations. So the next time you work with directories in your code, make sure to include this check. Happy coding!
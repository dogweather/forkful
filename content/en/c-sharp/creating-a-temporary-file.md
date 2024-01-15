---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Creating temporary files is a common practice in programming to store temporary data or maintain information during the execution of a program. Temporary files are typically deleted once the program has completed its task. 

## How To 
To create a temporary file in C#, you can use the `System.IO.Path.GetTempFileName()` method. This method creates a unique temporary file name that can then be used to create and write data to the file. Here's an example: 

```C#
string tempFile = System.IO.Path.GetTempFileName();
// Write some data to the file
using (System.IO.StreamWriter writer = System.IO.File.AppendText(tempFile))
{
    writer.WriteLine("This is a temporary file.");
}
```

To read the contents of the temporary file, you can use the `System.IO.File.ReadAllLines()` method. Here's another example: 

```C#
string[] tempFileContents = System.IO.File.ReadAllLines(tempFile);
```

The temporary file created with the `GetTempFileName()` method will be automatically deleted when the program terminates. However, if you need to delete the file before that, you can use the `System.IO.File.Delete()` method. 

## Deep Dive 
When you create a temporary file, it is typically stored in the system's temporary folder. You can find the path to this folder using the `System.IO.Path.GetTempPath()` method. 

It is important to note that temporary files should only be used for temporary storage. They are not meant to hold sensitive or important data as they can be accessed and modified by other processes. Additionally, when working with temporary files, it is a good practice to handle any exceptions that may be thrown and properly dispose of any resources being used. 

## See Also 
- [Microsoft Docs on GetTempFileName() Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- [Microsoft Docs on ReadAllLines() Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalllines)
- [Microsoft Docs on Delete() Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.delete)
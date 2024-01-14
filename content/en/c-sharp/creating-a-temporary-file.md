---
title:    "C# recipe: Creating a temporary file"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
Creating a temporary file is a common task in programming that serves a specific purpose. It allows developers to store data temporarily while it is being used or manipulated before being permanently saved or deleted. This can be beneficial in situations where the data is only needed temporarily or when there is a need for a backup of the original data.

## How To
```C#
// Create a temporary file
string tempFilePath = Path.GetTempFileName();

// Write data to the temporary file
using (StreamWriter writer = new StreamWriter(tempFilePath))
{
    writer.WriteLine("This is a temporary file.");
}

// Read from the temporary file
using (StreamReader reader = new StreamReader(tempFilePath))
{
    string data = reader.ReadToEnd();
    Console.WriteLine(data);
}

// Delete the temporary file
File.Delete(tempFilePath);
```

Output:
```
This is a temporary file.
```

## Deep Dive
Creating a temporary file in C# is a straightforward process. The `Path.GetTempFileName()` method generates a unique file name and creates a zero-byte file in the user's temporary folder. This method also returns the full path of the temporary file, which allows for easy manipulation and deletion.

In the above example, we used the `StreamWriter` class to write data to the temporary file. This class allows us to write text to files in a specific encoding. Similarly, the `StreamReader` class was used to read from the temporary file. It reads characters from a byte stream in a particular encoding.

It is essential to note that temporary files are not automatically deleted when the program terminates. As shown in the example, the `File.Delete()` method must be called to remove the temporary file after it has been used.

## See Also
- [MSDN: Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- [MSDN: StreamWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [MSDN: StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
---
title:                "Creating a temporary file"
html_title:           "PowerShell recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating temporary files is a common practice among programmers where they need to store and manipulate temporary data during the execution of a program. Temporary files are used to temporarily store data that may not be needed after the program finishes running.

## How to:

PowerShell provides the `New-TemporaryFile` cmdlet for creating temporary files. Simply use the following command to create a temporary file:

```PowerShell
New-TemporaryFile
```

You can also specify the file name and extension using the `-Name` parameter:

```PowerShell
New-TemporaryFile -Name "temp.txt"
```

The cmdlet will return a `System.IO.FileInfo` object representing the temporary file which you can then use for further operations. For example, you can read and write to the file using `Get-Content` and `Set-Content` cmdlets respectively.

```PowerShell
$tempFile = New-TemporaryFile

#Write to the file
"Hello World!" | Set-Content $tempFile

#Read from the file
Get-Content $tempFile

#Output:
#Hello World!
```

The temporary file will be automatically deleted when the PowerShell session ends or when you manually delete the file.

## Deep Dive:

Creating temporary files has been a common practice for a long time, even before the days of PowerShell. It is used to store data that does not need to be permanently stored and taking up space on the system.

While the `New-TemporaryFile` cmdlet is a convenient way to create a temporary file in PowerShell, it is not the only option. Some developers prefer to use the .NET `System.IO.Path` class to generate temporary file names and then use the `New-Item` cmdlet to create the file.

```PowerShell
$tempFileName = [System.IO.Path]::GetTempFileName()
New-Item -Path $tempFileName -ItemType File
```

Alternatively, you can also use the `System.IO.File` class to create and write to temporary files.

```PowerShell
$tempFile = [System.IO.Path]::GetTempFileName()
[System.IO.File]::WriteAllText($tempFile, "Hello World!")
```

It is important to note that creating a temporary file is not without risks. If your program relies heavily on temporary files, it can consume a significant amount of disk space and create performance issues. It is best to use temporary files sparingly and delete them as soon as they are no longer needed.

## See Also:

- [New-TemporaryFile documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
- [.NET System.IO.Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [.NET System.IO.File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)
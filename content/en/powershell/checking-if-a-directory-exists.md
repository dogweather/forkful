---
title:                "Checking if a directory exists"
html_title:           "PowerShell recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is a way to verify if a specified directory is present in a file system. This is a common practice among programmers to ensure that their scripts or programs run without errors and to avoid potential issues that may arise from accessing non-existent directories.

## How to:
```powershell
# Using the Test-Path cmdlet
Test-Path "C:\Users\Documents\NewFolder" 

Output: False 

# Using the .NET class System.IO.Directory
[System.IO.Directory]::Exists("C:\Users\Documents\NewFolder")

Output: False
```

The Test-Path cmdlet and the .NET class System.IO.Directory are two ways to check if a directory exists in PowerShell. Both methods return a boolean value of True or False, depending on whether the specified directory exists or not. These methods are commonly used in scripts and programs to handle files and perform operations on directories.

## Deep Dive:
Before the introduction of PowerShell, batch scripts were commonly used for automating routine tasks and file operations. To check if a directory exists in a batch script, the "if exist" conditional statement was used. With PowerShell, the Test-Path cmdlet was introduced, providing a more direct and efficient way to check for directory existence.

As an alternative to using cmdlets or .NET classes, one could use WMI (Windows Management Instrumentation) to check if a directory exists. This method involves querying the CIM_DataFile class with the specified directory path. However, it is not as straightforward as using the Test-Path cmdlet or the .NET class System.IO.Directory and may not be suitable for all scenarios.

When using the Test-Path cmdlet, it is important to specify the full path of the directory, including the drive letter, if applicable. Otherwise, the cmdlet will return False even if a folder with that name exists in the current working directory. The same applies when using the .NET class System.IO.Directory.

## See Also:
- [Microsoft Docs on Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [Microsoft Docs on System.IO.Directory](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [WMI query for checking if a directory exists (StackOverflow)](https://stackoverflow.com/questions/662190/directory-exists-using-wmi)
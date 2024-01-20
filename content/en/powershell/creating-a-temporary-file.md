---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file enables saving temporary data without manipulating your main files. Programmers do this for reasons like processing large volumes of data, creating backups, and testing their code.

## How to:

To create a temporary file, we use the New-TemporaryFile cmdlet. PowerShell makes this super easy. Notice the automatic random name it assigns to the file, which ensures it's unique. Let's see it in action:

```PowerShell
# Creating a Temporary File
$tempFile = New-TemporaryFile
# Outputting the Temporary File Name
$tempFile.FullName
```
Running this will produce an output similar to:

```PowerShell
C:\Users\Username\AppData\Local\Temp\tmp7681.tmp
```

Let's save some data in this file:

```PowerShell
# Write to the Temporary File
Set-Content -Path $tempFile.FullName -Value "Hello, World!"
```
And now let's read it back:

```PowerShell
# Read from the Temporary File
Get-Content -Path $tempFile.FullName
```

You'll now see:

```PowerShell
Hello, World!
```

## Deep Dive:

New-TemporaryFile was introduced in PowerShell 5.0, earlier versions require manual file initiation and some ugly code. The cmdlet creates a zero-byte, non-sparse file in the TEMP folder with.tmp file extension. 

As an alternative, you could use .NET's Path.GetTempFileName(), which effectively does the same thing, but it's a bit more cumbersome. Not to mention, you need to remember to import the System.IO namespace first.

```PowerShell
[System.IO.Path]::GetTempFileName()
```

The implementation of creating a temporary file is simple - it just picks a random unused name, creates the file in the TEMP folder, and opens it. It repeats this until it finds a usuable name or it hits its limit of 65,535 tries. 

## See Also:

1. [PowerShell Basic Cheat Sheet](https://devblogs.microsoft.com/scripting/powershell-basic-cheat-sheet/)
2. [Understanding the PowerShell New-TemporaryFile Cmdlet](https://adamtheautomator.com/new-temporaryfile-powershell/)
3. [New-TemporaryFile Microsoft Official Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7.1)
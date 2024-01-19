---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# A Simple Guide to Reading Text Files in PowerShell

## What & Why?

Reading a text file in PowerShell essentially involves fetching and interpreting data from a .txt file. Programmers do it to extract, analyze, or transform information stored in these files efficiently.

## How to:

Two basic cmdlets in PowerShell to read text files are `Get-Content` and `StreamReader`. Let's use them in some examples.

```PowerShell
# Using "Get-Content"
Get-Content -Path 'C:\Path\to\yourFile.txt'
```

This code reads and displays the content of 'yourFile.txt'. Straightforward, isn't it?

Now, let's fetch specific lines:

```PowerShell
Get-Content -Path 'C:\Path\to\yourFile.txt' | Select-Object -First 5
```

Replace "5" to fetch the number of lines you want.

Now, let's use `StreamReader`:

```PowerShell
$streamReader = New-Object System.IO.StreamReader('C:\Path\to\yourFile.txt')
$line = $streamReader.ReadLine()
$streamReader.Close()
```

This code reads the first line of 'yourFile.txt'. Result? It’s more resource-friendly for large files.

## Deep Dive

Historically, PowerShell (since version 1.0) has provided an in-built way to interact with files. As PowerShell is built on .NET, it inherited the `StreamReader` class, however, `Get-Content` (alias `gc`, `type`, `cat`) is often the favored approach as it's more 'PowerShelly'. Remember, `StreamReader` needs extra steps (like explicit closing) but is lighter for big files.

There are also other alternatives like `Import-Csv` for CSV files, and `convertFrom-Json` when dealing with JSON.

Furthermore, `Get-Content` does have a `-ReadCount` parameter which can help when dealing with large files as it sends data down the pipeline in chunks rather than line by line. 

## See Also
- Microsoft's official guide on `Get-Content`: [Here](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- Official info on `StreamReader`: [Here](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- An entertainingly-written guide on handling large files: [Here](https://www.powershellgallery.com/packages/PSReadLine/2.0.3)
- A detailed Stack Overflow discussion on the topic: [Here](https://stackoverflow.com/questions/46506586/how-to-read-large-text-files-in-powershell)
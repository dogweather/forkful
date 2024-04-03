---
date: 2024-01-20 17:55:00.852821-07:00
description: "Reading a text file means pulling its content into a form your program\
  \ can work with. Programmers do it for data processing, configuration, logging \u2013\
  \u2026"
lastmod: '2024-03-13T22:45:00.300141-06:00'
model: gpt-4-1106-preview
summary: Reading a text file means pulling its content into a form your program can
  work with.
title: Reading a text file
weight: 22
---

## How to:
Let's roll with the basics! Here's how you read from a text file in PowerShell:

```PowerShell
# Get the content of a file
$content = Get-Content -Path "C:\path\to\your\file.txt"
# Display the content in the console
Write-Output $content
```

Sample output might look like this if your file contained a couple lines of text:
```
Hello, PowerShell!
End of file.
```

Now, want to read line by line?

```PowerShell
# Read the file line by line
$lines = Get-Content -Path "C:\path\to\your\file.txt" -ReadCount 0
foreach ($line in $lines) {
    Write-Output $line
}
```

Same sample output as above, but processed one line at a time.

## Deep Dive
Long before PowerShell, command-line tools like `cat` in UNIX-like systems or `type` in DOS were the go-to for reading files. PowerShell's Get-Content is the sharp tool for this today, with added perks like reading line by line, which helps in avoiding memory overload with huge files. 

Beyond `Get-Content`, we've got `.NET` classes up our sleeves for more control – enter `System.IO.StreamReader`:

```PowerShell
$stream = [System.IO.StreamReader] "C:\path\to\your\file.txt"
try {
    while ($line = $stream.ReadLine()) {
        Write-Output $line
    }
}
finally {
    $stream.Close()
}
```

This is a more memory-efficient method, helpful for massive text mountains.

Alternatives? Well, you could use `Import-Csv` for CSV files or `ConvertFrom-Json` for JSON, if you want to scoop data into structured objects. But stick to `Get-Content` for the raw text stuff.

## See Also
Check out the official docs for more treasures:

- [Get-Content Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables) - This gives insight into variables like `$_`, which can be handy for inline processing.
- [Using PowerShell's .NET capabilities](https://docs.microsoft.com/en-us/powershell/scripting/developer/hosting/adding-and-invoking-commands?view=powershell-7.1) - For those diving deeper into the .NET framework within PowerShell.

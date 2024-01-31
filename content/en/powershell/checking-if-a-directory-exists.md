---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:58:01.879406-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is simply verifying if a folder is present at a specified path in the filesystem. Programmers do this to prevent errors, manage files efficiently, and ensure that data is being written or read from the correct locations.

## How to:

Use the `Test-Path` cmdlet to check for a directory's existence. This cmdlet returns a boolean: `$true` if the directory exists, and `$false` if it doesn't.

```PowerShell
# Check if a directory exists
$directoryPath = "C:\ExampleFolder"
$exists = Test-Path $directoryPath
Write-Output $exists  # Outputs True or False
```

Sample Output:
```
True
```
or if the directory doesn't exist:
```
False
```

You can also use it directly in an `if` statement:

```PowerShell
# Using Test-Path in an if statement
if (Test-Path $directoryPath) {
    Write-Output "Yep, it's there."
} else {
    Write-Output "Nope, can't find it."
}
```

## Deep Dive

The `Test-Path` cmdlet has been around since PowerShell v1.0. It's not just a one-trick pony; alongside directories, it can be used to check files, registry keys, and other items via different 'paths'.

There are alternatives. PowerShell is built on the .NET Framework, so you could drop down into .NET methods if you wanted:

```PowerShell
[system.io.directory]::Exists($directoryPath)
```

This serves the same purpose but goes about it the "long way 'round." Why bother, when `Test-Path` is built for the job?

Implementation-wise, checking for a directory before performing operations is best practice. It's about being predictable. You wouldn't drag race with an empty gas tank, right? So you don't read from or write to a nonexistent directory, either.

## See Also

For more info, peruse these links:

- [Test-Path Cmdlet Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [.NET Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)

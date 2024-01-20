---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

"Checking if a directory exists" means verifying the presence of a specific folder within a particular file system using a programming script, like PowerShell. Checking directories is crucial to prevent errors in scripts that rely on the existence of specific directories for reading files, storing data, etc.

## How to:

Checking whether a directory exists in PowerShell is straightforward. You use the `Test-Path` cmdlet, which returns `$True` if a specified path exists and `$False` otherwise.

Here's an example:

```PowerShell
$directoryPath = "C:\SomeDirectory"
if (Test-Path $directoryPath) {
    echo "This directory exists."
} else {
    echo "This directory does not exist."
}
```

If the directory, "C:\SomeDirectory", exists, this script will output: 

```PowerShell
This directory exists.
```

If the directory does not exist, the output will be:

```PowerShell
This directory does not exist.
```

## Deep Dive

The `Test-Path` cmdlet has been a part of PowerShell since its initial release in 2006, aiding scripts to work efficiently by avoiding file and directory related errors. 

An alternative of checking directory in PowerShell is with the `Get-ChildItem` cmdlet; however, it's less efficient as it fetches all item details.

Then there's the old `if exist` command from cmd.exe compatible with PowerShell. Yet, it has less flexibility compared to `Test-Path`.

While these alternatives can be used, `Test-Path` is the recommended method; it exploits the object-based system of PowerShell and involves less computation than other methods.

## See Also

For more details, check out the official Microsoft documentation:

1. [`Test-Path` - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
2. [`Get-ChildItem` - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7.1)

For cmd.exe command usage in PowerShell:
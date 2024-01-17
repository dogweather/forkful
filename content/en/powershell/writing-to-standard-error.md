---
title:                "Writing to standard error"
html_title:           "PowerShell recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error in PowerShell is a way to send error messages to the console, rather than outputting them as data. This is useful for troubleshooting and debugging code. Programmers do this to easily differentiate between regular data output and error messages, making it easier to identify and resolve issues in their code.

## How to:
To write to standard error in PowerShell, use the `Write-Error` command, followed by the error message in quotes. For example:

```PowerShell
Write-Error "Uh oh, something went wrong!"
```

This will display the error message in red text in the console.

You can also write to standard error from within a script by using the `$ErrorActionPreference` variable. This determines how PowerShell handles error messages. Setting it to `"Stop"` will write the error to the console, while setting it to `"SilentlyContinue"` will suppress the error message.

```PowerShell
$ErrorActionPreference = "Stop"
```

## Deep Dive:
Writing to standard error is not a new concept in coding and has been used in other languages, such as C and Java, for years. In PowerShell, it is considered a best practice to write error messages to standard error rather than standard output.

An alternative to writing to standard error is to use try-catch blocks. However, this can be time-consuming and cumbersome, especially for longer scripts. Writing to standard error is a more efficient way to handle errors in PowerShell code.

## See Also:
For more information on writing to standard error in PowerShell, check out the Microsoft documentation on [Error Handling in PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/07-error-handling?view=powershell-7.1).

You can also learn more about the `$ErrorActionPreference` variable and other error handling techniques in the article [Handling Errors the PowerShell Way](https://adamtheautomator.com/powershell-try-catch/).
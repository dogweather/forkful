---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) sends error messages and diagnostics separately from standard output (stdout). Programmers do it to cleanly separate regular program output from error information, making debugging and logging easier.

## How to:
```PowerShell
# Write a simple error to stderr
Write-Host "Oops, an error occurred!" -ForegroundColor Red 1>&2

# Write an error using Write-Error cmdlet
Write-Error "This is an error message!"

# Using $ErrorView to display or handle errors differently
$ErrorView = "CategoryView"
try {
    Get-ChildItem "nonexistentfile.txt"
} catch {
    Write-Host $_.Exception.Message -ForegroundColor Red 1>&2
}
```

Sample output:
```
Oops, an error occurred!
Write-Error: This is an error message!
Get-ChildItem: Cannot find path 'C:\...\nonexistentfile.txt' because it does not exist.
```

## Deep Dive
Historically, segregating stdout and stderr has Unix roots, letting users redirect outputs separately. PowerShell, which inherits this concept, uses Write-Error and Write-Host (with a redirection), amongst other cmdlets, to send messages to stderr. Under the hood, PowerShell wraps .NET methods to implement this feature.

Alternatives include using throw statements or exception handling blocks; however, these affect script flow. Writing to stderr does not interrupt execution unless you specifically check $Error variable or use -ErrorAction parameters.

## See Also
- [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)

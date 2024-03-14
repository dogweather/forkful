---
date: 2024-01-20 17:56:46.323173-07:00
description: "Reading command line arguments lets scripts behave differently based\
  \ on inputs outside the code. Programmers use them because they make scripts flexible,\u2026"
lastmod: '2024-03-13T22:45:00.298417-06:00'
model: gpt-4-1106-preview
summary: "Reading command line arguments lets scripts behave differently based on\
  \ inputs outside the code. Programmers use them because they make scripts flexible,\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets scripts behave differently based on inputs outside the code. Programmers use them because they make scripts flexible, usable in many scenarios without editing the code itself.

## How to

PowerShell reads command line arguments using the `$args` array or parameters. `$args` is quick for one-off scripts; parameters are better for robust tools.

### Using `$args`
```PowerShell
# myscript.ps1
Write-Host "You entered the following arguments:"
$args
```
Run with `.\myscript.ps1 Hello PowerShell`, outputs:
```
You entered the following arguments:
Hello PowerShell
```

### Using Parameters
```PowerShell
# myscriptparam.ps1
param (
    [string]$Name,
    [int]$Age
)
Write-Host "Hello, $Name! You are $Age years old."
```
Run with `.\myscriptparam.ps1 -Name Sarah -Age 32`, outputs:
```
Hello, Sarah! You are 32 years old.
```

## Deep Dive

PowerShell's modern approach to command line arguments is akin to a legacy from its predecessors like cmd and Bash. However, it amplifies flexibility and precision.

### Historical Context
Years back, batch files and shell scripts accessed arguments with numbered variables (like `%1`, `%2`). PowerShell refined this with `$args` and named parameters for more clarity and control.

### Alternatives
There are alternatives, like parsing raw input with `Read-Host` or accepting piped input. However, `$args` and parameters are more seamless for automated tasks and scripts.

### Implementation Details
`$args` is a simple array, good for arbitrary input. Parameters, with their attributes and types, can validate input and even prompt the user, making scripts self-documenting and less prone to errors.

## See Also

- [About Parameters](https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/cmdlet-parameter-sets?view=powershell-7)
- [Automatic Variables in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7&viewFallbackFrom=powershell-6)

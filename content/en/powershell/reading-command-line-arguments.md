---
title:                "Reading command line arguments"
html_title:           "PowerShell recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in PowerShell is the process of retrieving user-provided information from the command line when executing a script. Programmers do this in order to make their scripts more dynamic and customizable, as well as to automate repeated tasks.

## How to:

To read command line arguments in PowerShell, use the built-in $args variable. This variable contains an array of all the arguments provided by the user, separated by spaces.

```
# Sample script to read command line arguments

# Define variables for arguments
$arg1 = $args[0]
$arg2 = $args[1]

# Output arguments
Write-Host "Argument 1: $arg1"
Write-Host "Argument 2: $arg2"
```

Sample output when executing `.\script.ps1 argument1 argument2`:

```
Argument 1: argument1
Argument 2: argument2
```

## Deep Dive:

Historically, reading command line arguments was a common practice in shell scripting languages, as it allowed for more flexibility and customization in scripts. Nowadays, it is also commonly used in PowerShell for the same reasons.

An alternative way to read command line arguments in PowerShell is by using the `param()` statement, which allows for explicitly defining the expected arguments and their default values.

When implementing command line argument reading in PowerShell, it is important to handle unexpected or missing arguments, as well as to validate user input to avoid errors and unexpected behaviors.

## See Also:

- [Microsoft documentation on command line arguments](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters?view=powershell-7)
- [PowerShell script arguments tutorial](https://www.tutorialspoint.com/powershell/powershell_script_arguments.htm)
- [Comparison of $args and param() methods](https://devblogs.microsoft.com/scripting/when-to-use-param-and-when-to-use-args/)
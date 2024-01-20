---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Command-line arguments are inputs to a program, entered after the program name when starting it through the command line. They make programs flexible by allowing runtime customization, without any code changes.

## How to:

To read command line arguments in PowerShell, you use the automatic variable `$args`. Here's a basic script:

```PowerShell
# myScript.ps1
$args
```

If you run this script (`myScript.ps1 arg1 arg2 arg3`), you'll see the following output:

```PowerShell
arg1
arg2
arg3
```

You can also read specific arguments using index values:

```PowerShell
# myScript.ps1
"First argument: "+ $args[0]
"Second argument: "+ $args[1]
```

Running `myScript.ps1 Hello World` spits out:

```PowerShell
First argument: Hello
Second argument: World
```

Remember, indices start at 0!

## Deep Dive

Historically, command line arguments have been around since the early days of UNIX, providing a way to parameterize scripts and utilities.

In PowerShell, you might also see `param()`. This block defines specific parameters your script accepts. It's more structured, lets you set defaults, requires specific inputs, or make parameters optional.

```PowerShell
param (
  [Parameter(Mandatory=$true)][string]$name,
  [int]$age = 25
)
Write-Host "Name is $name, and age is $age"  
```

Running the above code as `myScript.ps1 -name John -age 30`, you'd get:

```PowerShell
Name is John, and age is 30
```

Though more verbose, `param()` offers greater control and error checking than `$args`.

## See Also

- [Microsoft's guide on about_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters?view=powershell-7.1) provides more detail about `param()`.
- [This StackOverflow thread](https://stackoverflow.com/q/21503865) discusses `$args` and `param()` differences.
- [Computer Hope's page](https://www.computerhope.com/jargon/c/commandi.htm) gives insights into the history of command-line arguments.
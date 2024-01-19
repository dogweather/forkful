---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generating Random Numbers with PowerShell

## What & Why?
Generating random numbers simply means creating a number that can't be logically predicted. Programmers do it to introduce randomness in their code for scenarios like password generation, statistical sampling, simulation, and testing. 

## How to:
PowerShell makes it very easy to generate random numbers using the `Get-Random` cmdlet. Here's how:

To generate a random number between 0 and 10, you would use:

```PowerShell
Get-Random -Maximum 10
```

Sample output would be something like `7`, depending on how your stars align today.

If you want to specify a range, say between 10 and 20, use:

```PowerShell
Get-Random -Minimum 10 -Maximum 20
```

Sample output could be `14`, `19`, or `10`. Don't take my word for it, though - your output may differ.

## Deep Dive
The `Get-Random` cmdlet in PowerShell makes use of the .NET System.Random class, which implements a pseudo-random number generator. Although it works totally fine for most practical use-cases, it's not ideal for applications requiring cryptographically secure random numbers.

For historical context, before PowerShell and .NET, scripting languages like Visual Basic Script didn't provide built-in functionality for generating random numbers. Developers had to manually implement a random number generator using various algorithms.

An alternative would be using the .NET classes directly. For instance, you could create a new object of the System.Random class to generate random numbers by using `$Random = New-Object -TypeName System.Random` command. But `Get-Random` is indeed more PowerShell-ish, and does the same job in less code.

## See Also
Check out these resources to learn more about random numbers in PowerShell and .NET:
- PowerShell `Get-Random` cmdlet: [Official Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
- .NET `System.Random` class: [Official Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- Best practices for randomness in .NET: [Microsoft Security Blog](https://cloudblogs.microsoft.com/microsoftsecure/2014/02/19/the-dangers-of-key-reuse-random-number-generation-and-low-entropy/)
---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:29.976810-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are unpredictable values. In programming, we use them for stuff like testing, security, and simulating unpredictable events.

## How to:
PowerShell makes generating random numbers easy-peasy. Let's see it in action.

```PowerShell
# Basic random number between 0 and Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber

# Random number within a specified range
$randomInRange = Get-Random -Minimum 10 -Maximum 50
Write-Output $randomInRange

# Random selection from an array
$array = 1..10
$randomElement = Get-Random -InputObject $array
Write-Output $randomElement
```

Sample output:
```
1073741823
27
8
```

To get consistent results (handy for testing), set the seed:

```PowerShell
# Setting the seed for reproducibility
[Random]::new(11).Next(1, 100)
```

## Deep Dive
Way back, random numbers were generated manually—which was a hassle. Then computers stepped in. In PowerShell, the `Get-Random` cmdlet uses a pseudo-random number generator (PRNG). A PRNG uses algorithms to spit out number sequences that appear random but aren't truly unpredictable (hence "pseudo").

Alternatives? Sure. You could tap into .NET's `[Random]` class for more control, like the seed example above. Or delve into cryptography with `[System.Security.Cryptography.RandomNumberGenerator]` for more robust randomness.

Implementation-wise, computers rely on math for "randomness," often involving complex algorithms like the Mersenne Twister. The catch is, it starts with a seed value. Change the seed, and you change the sequence. Hence, the reproducibility.

## See Also
- PowerShell docs on `Get-Random`: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- Microsoft's .NET `[Random]` class: [link](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- Cryptographically secure random numbers: [link](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator)
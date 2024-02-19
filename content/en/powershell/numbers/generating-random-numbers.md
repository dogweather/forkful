---
aliases:
- /en/powershell/generating-random-numbers/
date: 2024-01-27 20:26:33.570342-07:00
description: "Generating random numbers in PowerShell is about creating unpredictable\
  \ numeric values within a specified range. Programmers utilize this capability for\
  \ a\u2026"
lastmod: 2024-02-18 23:09:11.267863
model: gpt-4-0125-preview
summary: "Generating random numbers in PowerShell is about creating unpredictable\
  \ numeric values within a specified range. Programmers utilize this capability for\
  \ a\u2026"
title: Generating random numbers
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in PowerShell is about creating unpredictable numeric values within a specified range. Programmers utilize this capability for a myriad of reasons, including testing, simulation, and security purposes, where unpredictability or mimicking real-world randomness is crucial.

## How to:
PowerShell offers a straightforward approach to generate random numbers using the `Get-Random` cmdlet. This cmdlet can produce random numbers within a default range or a specified range.

```PowerShell
# Generate a random number between 0 and Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

To specify a range, use the `-Minimum` and `-Maximum` parameters:

```PowerShell
# Generate a random number between 1 and 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

For more control, you can instantiate an object of the `System.Random` class:

```PowerShell
# Using System.Random for a sequence of numbers
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

If you need a random selection from an array or collection, `Get-Random` can directly pick an item:

```PowerShell
# Random selection from an array
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Deep Dive
The `Get-Random` cmdlet in PowerShell leverages the .NET class `System.Random` under the hood to generate pseudorandom numbers. These are "pseudo" because they use algorithms to produce sequences of numbers that only appear random. For most applications, this level of randomness is sufficient. However, for use cases requiring cryptographic security, `System.Random` is not suitable due to its predictable nature.

PowerShell and .NET offer `System.Security.Cryptography.RNGCryptoServiceProvider` for cryptographic randomness, which is more appropriate for generating encryption keys or other security-sensitive operations:

```PowerShell
# Cryptographically secure random numbers
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

While `Get-Random` and `System.Random` satisfy a broad set of needs for randomness in scripting and application logic, it's essential to select the right tool for the job, especially in security-centric applications where predictability can present a vulnerability.

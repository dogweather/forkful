---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:09.950465-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in PowerShell involves creating numerical values that are unpredictable and differ each time your script runs. Programmers use this feature for a range of tasks, from generating unique identifiers to simulating data and creating secure keys or tokens.

## How to:

PowerShell offers a straightforward method to generate random numbers using the `Get-Random` cmdlet. You can specify a range or rely on the default behavior.

### Generating a Random Number within a Range

To create a random number between 1 and 100, you can use the `-Minimum` and `-Maximum` parameters:

```PowerShell
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Note: The `-Maximum` value is exclusive, hence `101` to include `100` in the potential outcomes.

### Generating a Random Number from an Array

If you have a set of specific values and want to select one at random, pass an array to `Get-Random`:

```PowerShell
$colors = 'Red', 'Green', 'Blue'
$randomColor = $colors | Get-Random
Write-Output $randomColor
```

### Secure Random Numbers

For cryptographic purposes or when you need more secure random numbers, you can utilize the `[System.Security.Cryptography.RandomNumberGenerator]` class:

```PowerShell
$bytes = New-Object "System.Byte[]" 4  # Array of 4 bytes
[System.Security.Cryptography.RandomNumberGenerator]::Fill($bytes)
$secureRandomNumber = [System.BitConverter]::ToInt32($bytes, 0)
Write-Output $secureRandomNumber
```

This technique generates a secure random integer.

## Deep Dive

Historically, generating random numbers in programming languages, including PowerShell, has relied on algorithms that produce sequences of numbers that only appear to be random, also known as pseudo-random number generators (PRNGs). The `Get-Random` cmdlet in PowerShell is based on such a PRNG and is sufficient for general-purpose tasks where true randomness is not critical.

However, when cryptographic security is necessary, as in the generation of encryption keys or secure tokens, pseudo-random numbers may not suffice due to their predictable nature when the algorithm or seed value is known. This is why the `[System.Security.Cryptography.RandomNumberGenerator]` class is recommended for secure applications. It interfaces with system-level cryptographic services to produce cryptographically secure random numbers, which are far less predictable and thus more suitable for security-sensitive tasks.

It's worth noting that while `Get-Random` is convenient and sufficient for many use cases, understanding the underlying nature of the randomness it produces is crucial for applications where security and unpredictability are paramount. For such scenarios, always consider using cryptographically secure alternatives available in the .NET Framework, accessible from within PowerShell.

## See also

### Official PowerShell Documentation
- [Get-Random Cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)

### Tutorials and Guides
- **TechNet**: [PowerShell for beginners: Scripts and loops](https://social.technet.microsoft.com/wiki/contents/articles/183.windows-powershell-survival-guide.aspx)
  - **Generating Random Numbers**: Focus on the section about using `Get-Random`.

### Community Guides and Blogs
- **Adam the Automator**: [Learn to Use the PowerShell Get-Random Cmdlet](https://adamtheautomator.com/powershell-get-random/)
- **SS64**: [PowerShell Get-Random command](https://ss64.com/ps/get-random.html)

### Code Examples
- **GitHub Gist**: [PowerShell Random Number Generator Function](https://gist.github.com/jokecamp/5fab6d3f228a2f5d8d5c)
- **Stack Overflow**: [How to generate a random number in PowerShell](https://stackoverflow.com/questions/11887940/how-to-generate-a-random-number-in-powershell)

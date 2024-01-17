---
title:                "Generating random numbers"
html_title:           "PowerShell recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is the process of producing a sequence of numbers that appear to be random. Programmers often use this technique in their code to add an element of randomness or unpredictability to their programs. This can be useful in many applications such as games, simulations, and security algorithms.

## How To:

```PowerShell
# Generating a single random number between 1 and 10
Get-Random -Minimum 1 -Maximum 10 

# Generating multiple random numbers between 0 and 100
Get-Random -Count 5 -Maximum 100
```

Sample Output:
```
8
42 15 93 1 77
```

## Deep Dive:

### Historical Context

The concept of generating random numbers has been around for centuries, with the earliest known methods dating back to ancient China and Egypt. However, the first computer-generated random numbers didn't appear until the 1940s when scientists were using electronic computers for simulations and numerical analysis.

### Alternatives

While PowerShell's `Get-Random` cmdlet is an easy and efficient way to generate random numbers, there are other options available. One alternative is using the `System.Random` class, which gives more control over the generation of random numbers. Another option is to use a third-party module, such as PSRandom, for more advanced random number generation techniques.

### Implementation Details

PowerShell's `Get-Random` cmdlet uses the .NET Framework's underlying `System.Random` class to generate random numbers. It uses a mathematical formula called a pseudo-random number generator that produces a sequence of numbers that appear random. Keep in mind that these numbers are not truly random, as they are generated using a deterministic algorithm.

## See Also:

- [Get-Random cmdlet documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7)
- [System.Random class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [PSRandom module information](https://docs.microsoft.com/en-us/powershell/module/psrandom/?view=powershell-7)
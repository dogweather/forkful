---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:32.494695-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Random numbers are surprise elements in a predictable system. Programmers use them for everything from security (like generating keys) to gaming logic (like dice rolls).

## How to (Як це зробити):
In PowerShell, you can summon random numbers easily. Roll a virtual die or pick a secret number with these snippets:

```PowerShell
# Roll a die
Get-Random -Minimum 1 -Maximum 7

# Generate a random number between 0 and 100
Get-Random -Maximum 100
```

Sample output:

```
6
42
```

These get you unpredictability in a snap.

## Deep Dive (Поглиблено):
Historically, true randomness was challenging. Early programs used algorithms that mimicked randomness, known as pseudo-random number generators (PRNGs).

In PowerShell, `Get-Random` taps into .NET's `System.Random` class by default, which is a PRNG. It's sufficient for most tasks but not for high-security needs.

Alternatives include tapping directly into the cryptographic class `[System.Security.Cryptography.RandomNumberGenerator]` for more security-sensitive use cases.

Remember, the upper limit number in `Get-Random -Maximum` is exclusive—you never get that number.

## See Also (Дивіться також):
- Learn more about `[System.Random]`: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0
- For advanced secure random numbers, `[System.Security.Cryptography.RandomNumberGenerator]`: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0
- About randomness in computing: https://en.wikipedia.org/wiki/Randomness

No fluff. Get coding, get random! 🎲
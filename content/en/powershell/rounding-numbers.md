---
title:                "Rounding numbers"
date:                  2024-01-25T02:59:40.250262-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers is about tuning a value to the nearest integer or specified decimal place. Programmers round numbers to simplify data, enhance readability, or meet certain mathematical requirements during calculations.

## How to:
You've got a few handy cmdlets and methods in PowerShell for rounding:

- `Round()` method from Math class
```PowerShell
[Math]::Round(15.68) # Rounds to 16
```
- Specify decimals:
```PowerShell
[Math]::Round(15.684, 2) # Rounds to 15.68
```
- `Ceiling()` and `Floor()`, for always rounding up or down:
```PowerShell
[Math]::Ceiling(15.2) # Rounds up to 16
[Math]::Floor(15.9) # Rounds down to 15
```

## Deep Dive
Rounding numbers is no newcomer; it's been around since ancient times, useful for trade, science, and timekeeping. Talking about PowerShell, `[Math]::Round()` follows "Banker's Rounding" by default, where 0.5s goes to the nearest even number, reducing bias in statistical operations.

You're not just stuck with `[Math]` methods though. Want more control? Check out `[System.Math]::Round(Number, Digits, MidpointRounding)` where you can set how midpoints are handled: away from zero or to even (aka Banker’s Rounding).

Another angle: the `System.Globalization.CultureInfo` object. It helps with locale-specific formatting and rounding preferences when dealing with international numbers.

## See Also
- Microsoft's official docs on Math methods: [Link](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- Decimal rounding specifics in .NET: [Link](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- Discussions on rounding in StackOverflow: [Link](https://stackoverflow.com/questions/tagged/rounding+powershell)

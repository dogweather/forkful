---
date: 2024-01-20 17:42:49.447845-07:00
description: 'How to: PowerShell uses the `-replace` operator to delete characters
  that match a pattern. Here''s some string-fixing action for you.'
lastmod: '2024-03-13T22:45:00.267768-06:00'
model: gpt-4-1106-preview
summary: PowerShell uses the `-replace` operator to delete characters that match a
  pattern.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
PowerShell uses the `-replace` operator to delete characters that match a pattern. Here's some string-fixing action for you:

```PowerShell
# Simple replacement: removing digits
$text = 'ABC123'
$cleanText = $text -replace '\d+'
$cleanText  # Outputs: ABC

# Getting rid of whitespace
$text = 'Hello World         '
$trimmedText = $text -replace '\s+$'
$trimmedText  # Outputs: Hello World

# Nixing specific characters
$text = 'uN_w@nt3d-charact3r$'
$cleanedUpText = $text -replace '[-@3$]', ''
$cleanedUpText  # Outputs: uNwntd-charactr
```

## Deep Dive
PowerShell's `-replace` operator is a mighty tool that harnesses regex (regular expressions). Regex is an almost arcane art; it's been around since the 1950s and works across many programming languages for pattern matching.

Alternatives to `-replace`? For straightforward stuff, there’s the `.Trim()` method family for white spaces and `.Replace()` method for literal replacements. But the `-replace` operator is your go-to for pattern-based operations.

Under the hood, when you use `-replace`, PowerShell taps into the .NET Framework's regex capabilities. It's a powerful match-and-slice operation that works on a per-character level to decide what stays and what goes. Remember, regex patterns can get complex and consume more processing power for intricate patterns, so use with care!

## See Also
To dive deeper into the regex rabbit hole, check out these:
- [PowerShell's About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [Automate the Boring Stuff with PowerShell](https://adamtheautomator.com/powershell-replace/) for real-world applications.

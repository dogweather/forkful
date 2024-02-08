---
title:                "Об'єднання рядків"
aliases:
- uk/powershell/concatenating-strings.md
date:                  2024-01-20T17:35:37.486752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Concatenating strings means sticking them together end-to-end. Programmers do it to assemble text dynamically, like creating messages or generating paths.

## How to: (Як це зробити:)
```PowerShell
# Using the + operator
$firstName = "Taras"
$lastName = "Shevchenko"
$fullName = $firstName + " " + $lastName
Write-Host $fullName # Output: Taras Shevchenko

# With the -f format operator
$greeting = "Hello, {0} {1}!"
Write-Host ($greeting -f $firstName, $lastName) # Output: Hello, Taras Shevchenko!

# Joining an array of strings with -join
$paths = 'C:\', 'Program Files', 'PowerShell'
$fullPath = $paths -join '\'
Write-Host $fullPath # Output: C:\Program Files\PowerShell
```

## Deep Dive (Поглиблене занурення):
Originally, computers just processed numerical data, but text processing became essential. String concatenation has been in programming languages since the early days, as a fundamental operation.

PowerShell has evolved from these concepts and adds its own spin. Using the `+` operator is straightforward but has performance considerations with large strings or in loops due to immutable string objects - each concatenation creates a new string.

The `-f` format operator and `-join` are alternatives to `+`. They can be cleaner and often faster. `-f` is perfect for templated strings while `-join` effortlessly brings arrays together.

Understanding the underlying .NET framework that PowerShell is based on is useful. String concatenation in .NET can involve multiple methods, such as StringBuilder class for heavy-duty operations, which PowerShell doesn't expose directly but is accessible through .NET interoperability.

## See Also (Дивіться також):
- [About Join](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_join)
- [About Operators](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_operators)

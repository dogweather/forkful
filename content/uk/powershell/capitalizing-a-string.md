---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Capitalizing a string means turning all the letters to uppercase. Programmers often do this to standardize text data, like making headers uniform or ensuring case-insensitive comparisons.

## How to: (Як це зробити:)
In PowerShell, capitalizing is straightforward. Use the `.ToUpper()` method. Here's how:

```PowerShell
$text = "hello, world"
$capitalizedText = $text.ToUpper()
Write-Host $capitalizedText
```

Sample output:

```
HELLO, WORLD
```

## Deep Dive (Поглиблений Розгляд)
Capitalizing strings in programming languages is a common task. In PowerShell, it's been simple since its inception, using the method mentioned above. The `.ToUpper()` method originates from .NET, which PowerShell is built upon.

Alternative ways to capitalize include:

- Using culture-specific methods if needed (like `.ToUpperInvariant()`).
- With PowerShell 7+, you can utilize the `ToUpperInvariant()` method, which ignores current culture settings for consistency across different systems.

The critical thing to remember is that capitalizing affects the original string's meaning and sort order in some cultures, so know your data before you transform it.

## See Also (Дивіться Також)
- Explore the .NET method details: [.NET String Methods](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- Understand how culture can affect string operations: [Culture in .NET](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/globalization)

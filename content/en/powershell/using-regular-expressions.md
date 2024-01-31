---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are powerful patterns for matching text strings. Programmers use regex to search, validate, or replace content efficiently.

## How to:
```PowerShell
# Match a pattern beginning with 'S' followed by any characters, ending in 'e'
$pattern = 'S.*e'
$text = 'Sample sentence in PowerShell.'
if ($text -match $pattern) {
    "Match found: $($matches[0])"
}

# Replace all occurrences of 'dog' with 'cat'
$petStory = 'The quick brown dog jumps over the lazy dog.'
$petStory -replace 'dog', 'cat'
```
Output:
```
Match found: Sample sentence in
The quick brown cat jumps over the lazy cat.
```

## Deep Dive
Regex has been integral to programming since the 1950s. While PowerShell has built-in cmdlets like `-match`, `-replace`, and `Select-String` for regex, alternatives for text manipulation exist – think `string.Contains` or `string.Replace`. PowerShell's regex uses the .NET framework's implementation, hence it's robust and feature-rich.

## See Also
- [Microsoft's official regex reference](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [Regex101: Build and test regex](https://regex101.com/)

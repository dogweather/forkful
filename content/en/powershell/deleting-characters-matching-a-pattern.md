---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern - a common task in programming - lets you fine-tune data strings just the way you need them. It's a useful tool for cleaning up the input or simplifying text processing.

## How to:

A practical example of deleting characters in a pattern in PowerShell is using the `-replace` operator with regular expressions (regex). Here's how it works:

```PowerShell
# Example string
$string = "Hello,****** World!"

# Replace all '*' characters
$newString = $string -replace '\*', ''

# Print the new string
$newString
```

After running the above code, the output on your PowerShell window is:

```PowerShell
Hello, World!
```

## Deep Dive

The `-replace` operator in PowerShell, used in the context of string manipulation, comes from its regex support, a powerful feature that originated from early Unix systems.

For alternatives, you could use the .NET-based `String.Replace()` method, but this lacks the regex power `-replace` offers:

```PowerShell
# Using .NET String.Replace()
$newStringDotNet = $string.Replace("*","")
```

Understanding PowerShell's implementation of regex includes knowing about its automatic multiline mode, where '^' and '$' match the start and end of any line within a string, not just the string itself. 

## See Also

For a deep dive into PowerShell's regex capabilities, check out these resources:
- [Microsoft's Regex Overview in PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/regular-expressions?view=powershell-7.1)
- [A Practical Guide to PowerShell Regex](https://adamtheautomator.com/powershell-regex)
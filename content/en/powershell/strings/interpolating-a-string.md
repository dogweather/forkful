---
title:                "Interpolating a string"
aliases: - /en/powershell/interpolating-a-string.md
date:                  2024-01-20T17:51:18.322254-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation slides values into a string template like pieces of a puzzle, making strings dynamic and code cleaner. Programmers use it to insert variables, expressions, and formatting directly within strings, cutting down on concatenation clutter.

## How to:
In PowerShell, you interpolate with double-quoted strings and the `$` symbol before the variable name. Wrap expressions in `$()` to evaluate them right inside the string.

```PowerShell
$name = "Alex"
$day = (Get-Date).DayOfWeek

# Basic variable interpolation
"Hello, $name! Happy $day!"

# Expression interpolation
"Pi to two decimal places is $(Math::Round([Math]::Pi, 2))"

# Output
Hello, Alex! Happy Wednesday!
Pi to two decimal places is 3.14
```

## Deep Dive
PowerShell adopted string interpolation borrowing from earlier programming languages like Perl. Before PowerShell v3, we concatenated with the `+` operator or used the `-f` format operator. Here's the evolution:

- Old-school concatenation: `"Hello, " + $name + "! It's " + $day + "."`
- Format operator: `"Hello, {0}! It's {1}." -f $name, $day`

Interpolated strings are easier to read and less error-prone. Under the hood, PowerShell interprets the interpolated string and replaces variables or expressions with their values when the string is evaluated, not when it's defined.

## See Also
- [Format operator explanation](https://ss64.com/ps/syntax-f-operator.html)

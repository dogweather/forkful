---
date: 2024-02-03 19:03:01.274172-07:00
description: "Regular expressions (regex) are sequences of characters that form a\
  \ search pattern, primarily used for string searching and manipulation. Programmers\u2026"
lastmod: '2024-03-13T22:45:00.273121-06:00'
model: gpt-4-0125-preview
summary: Regular expressions (regex) are sequences of characters that form a search
  pattern, primarily used for string searching and manipulation.
title: Using regular expressions
weight: 11
---

## How to:
In PowerShell, you can use the `-match`, `-replace`, and `-split` operators, among others, to perform actions with regular expressions. Let's explore a few examples:

### Using `-match` to check if a string matches a pattern
This operator returns `$true` if the pattern is found within the string, and `$false` otherwise.

```powershell
"hello world" -match "\w+orld"
# Output: True
```

### Extracting matches
You can extract the matched value by accessing the automatic variable `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Number found: " + $matches[0]
}
# Output: Number found: 100
```

### Using `-replace` for substitutions
The `-replace` operator replaces all occurrences of a pattern with a specified replacement string.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Output: foo qux qux
```

### Splitting strings with `-split`
Split a string into an array of substrings based on a regex pattern.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Output: The quick brown fox jumps
```

### Advanced Pattern Matching
PowerShell also supports more complex regex operations via the `[regex]` class, giving you access to methods like `Matches()`, `Replace()`, and `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Output: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Output: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Output: one two three four
```

These examples show the power and versatility of regular expressions in PowerShell for data manipulation and pattern matching. By harnessing regex, programmers can perform complex text processing efficiently.

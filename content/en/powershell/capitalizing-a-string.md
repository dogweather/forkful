---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means making each letter start with an upper case, commonly used for titles or to emphasize proper nouns. Programmers use it to format output or prepare data for display consistency.

## How to:
Let’s jazz up some text. In PowerShell, use `.ToTitleCase` from `System.Globalization` for title-like capitalization, or simple methods like `.ToUpper()` or `.ToLower()` to change case.

```PowerShell
# Load the TextInfo class to use ToTitleCase
$textInfo = (Get-Culture).TextInfo

# Title case example
$titleCaseString = $textInfo.ToTitleCase("hello, powershell aficionados!")
Write-Output $titleCaseString

# Output: Hello, Powershell Aficionados!

# Upper case example
$upperCaseString = "make me shout".ToUpper()
Write-Output $upperCaseString

# Output: MAKE ME SHOUT

# Lower case example
$lowerCaseString = "SILENCE IS GOLDEN".ToLower()
Write-Output $lowerCaseString

# Output: silence is golden
```

## Deep Dive
Capitalization comes from typographic tradition, where titles and proper nouns begin with uppercase letters. In computer programming, this practice entered for visual standardization and readability.

Technically, `.ToTitleCase` isn’t just about making letters uppercase. It follows rules, like not capitalizing conjunctions, prepositions, or articles in some contexts. Bet you didn’t expect that from a one-liner code snippet, huh?

Alternatives exist: regex can do funky case transformations, but it’s overkill for simple tasks. Plus, readability counts—`.ToTitleCase`, `.ToUpper()`, and `.ToLower()` tell you exactly what they do. No guesswork needed.

One detail: be wary of culture-specific rules affecting capitalization. For instance, "i" becomes "I" in English, but not so in other languages. This is where `TextInfo` shines; it respects cultural nuances.

## See Also
Check out these resources for a deeper dive:

- [Microsoft Docs on ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)

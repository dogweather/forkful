---
date: 2024-01-20 17:58:41.333882-07:00
description: 'How to: PowerShell makes search and replace pretty straightforward.
  Check out `-replace` for strings, and `Get-Content` with `Set-Content` for files.
  #.'
lastmod: '2024-03-13T22:45:00.268682-06:00'
model: gpt-4-1106-preview
summary: PowerShell makes search and replace pretty straightforward.
title: Searching and replacing text
weight: 10
---

## How to:
PowerShell makes search and replace pretty straightforward. Check out `-replace` for strings, and `Get-Content` with `Set-Content` for files.

### Replace text in a string:
```PowerShell
$text = "I love PowerShell"
$updatedText = $text -replace "love", "adore"
$updatedText
```
Sample Output:
```
I adore PowerShell
```

### Replace text in a file:
```PowerShell
$file = "example.txt"
$content = Get-Content $file
$content | ForEach-Object { $_ -replace "oldWord", "newWord" } | Set-Content $file
```
No output here, but `example.txt` now has every "oldWord" replaced with "newWord".

## Deep Dive
Since the dawn of text editing, search and replace has been a cornerstone. Think of it like the find-and-replace in a word processor but supercharged for coding needs.

Back in the day, command-line wizards used tools like `sed` in Unix. PowerShell brought this functionality into its scripting language. Why's it cool? Because it's bound to objects, not just text. That means you can tweak not only code and text files but also data structures and beyond.

Alternatives? Sure. You’ve got text editors and IDEs with their own find-and-replace, batch scripts, or even programming libraries designed for text manipulation.

Implementation details? PowerShell does regex. That means you can replace stuff based on patterns, not just fixed words. Plus, with PowerShell scripts, you can automate these operations across massive numbers of files, saving you a time load.

## See Also
- PowerShell `-replace` operator documentation: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- Using `Get-Content` and `Set-Content`: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)

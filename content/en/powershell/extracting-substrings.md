---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Ever found yourself needing a smaller piece of a larger string in your code? It's a situation we all encounter, and that's where extracting substrings comes in handy. It allows us to pick out specific portions of a string for a variety of purposes- parsing file paths, pulling out bits of user input, or checking for certain word patterns, among other uses. 

## How to:
In PowerShell, use the `substring()` method to extract substrings. It needs two arguments: 

- the start index (first character is 0)
- the length of the substring. 

Here's an example. Suppose we have a string, "PowerShell Rocks!" and we want to extract the word "Rocks". 

```PowerShell
$string = "PowerShell Rocks!"
$substr = $string.Substring(12, 5)
$substr 
```

You'll see that the output is:

```PowerShell
Rocks
```

We started at the 12th character and went five characters long, giving us "Rocks".

## Deep Dive
In PowerShell’s formative years, extracting substrings was less straightforward because it borrowed heavily from the .NET Framework. As developers sought simpler and smoother methods, the `substring()` method became a common tool in the PowerShell toolkit. 

Alternatives to this method exist. For example, you could use regex (regular expressions), which are more versatile but also more complex. When it comes to implementation details, keep in mind that indexing in PowerShell starts from 0, not 1. Also, if the length parameter exceeds the remaining characters, PowerShell will return all characters until the end of the string.

## See Also
If you want to delve further into manipulating strings in PowerShell, check out these resources:

- [Working with Strings](https://docs.microsoft.com/powershell/scripting/samples/working-with-strings?view=powershell-7.1): A practical guide to strings on the official PowerShell documentation.
- [About Regular Expressions](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1): If you fancy learning regex, this article is a good place to start.
- [Mastering everyday XML tasks in PowerShell](https://docs.microsoft.com/previous-versions//dd878284(v=vs.85)): Learn about handling XML, a situation where substring extraction often comes in handy.
---
title:                "Capitalizing a string"
html_title:           "PowerShell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# PowerShell Magic: Capitalizing Strings

## What & Why?
Capitalizing a string means to transform its first letter into uppercase. Programmers do it to ensure data consistency, enhance readability, or satisfy certain code requirements.

## How to:
Here's how to capitalize a string in PowerShell, using string manipulation and its powerful .NET framework integration.

```PowerShell
$string = 'hello world'
$capitalized = $string.Substring(0,1).ToUpper()+$string.Substring(1).ToLower()
Write-Output $capitalized
```
Here's your output:

```PowerShell
Hello world
```

## Deep Dive:
Historically, there's no built-in function in PowerShell for capitalizing strings. Why? Remember that PowerShell relies heavily on the .NET framework that originally didn't have this feature. But thanks to PowerShell's flexibility, we can use substring method and ToUpper function from .NET directly in PowerShell. Simple and effective!

Alternatives? One alternative to our code above is using TextInfo class from System.Globalization namespace:

```PowerShell
$string = 'hello world'
$textInfo = [System.Globalization.TextInfo]::CurrentCulture.TextInfo
$capitalized = $textInfo.ToTitleCase($string)
Write-Output $capitalized
```

Note: this method capitalizes every word in the string!

When it comes to the execution of these scripts, PowerShell directly utilizes .NET, meaning that in the back, C# code is being used. Anytime we're capitalizing strings, we're leveraging methods provided by the .NET string class, testifying to PowerShell's tight integration with .NET.

## See Also:
Get familiar with string manipulation in PowerShell with these superb resources:

- [Microsoft's `string` class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [PowerShell and the .NET framework](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-comparison?view=powershell-7.1)
- [Working with strings in PowerShell](https://www.cyberciti.biz/faq/powershell-split-string-into-array/)
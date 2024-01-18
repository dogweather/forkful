---
title:                "Parsing a date from a string"
html_title:           "PowerShell recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string in PowerShell refers to extracting the date information from a string of text and converting it into a valid date format that can be used in calculations or displayed in a specific format. Programmers do this in order to manipulate and use date information in their scripts and applications.

## How to:
To parse a date from a string in PowerShell, you can use the `Get-Date` cmdlet along with the `-Date` parameter. This allows you to specify the date string that you want to parse and the format in which it is written. See the example below:

```PowerShell
Get-Date -Date "12/25/2021"
```

The output of this code block would be: `Saturday, December 25, 2021 12:00:00 AM`.
Alternatively, you can use the `[DateTime]::ParseExact` method to manually specify the format of the date string and convert it into a `DateTime` object. See the example below:

```PowerShell
[DateTime]::ParseExact("2021-12-25","yyyy-MM-dd",$null)
```

The output of this code block would be: `Saturday, December 25, 2021 12:00:00 AM`.

## Deep Dive:
Parsing dates from strings has become increasingly important for programmers as more and more data is stored in databases and spreadsheets. Date information often needs to be extracted and manipulated in order to make informed decisions or perform calculations. In PowerShell, there are various methods for parsing dates, such as using built-in cmdlets like `Get-Date` or using .NET methods like `ParseExact`.

An alternative method for parsing dates in PowerShell is using regular expressions. This allows for more flexibility in handling different date formats and can be useful for data validation purposes. However, regular expressions can be more complex and may require a deeper understanding of pattern matching in order to use effectively.

## See Also:
- `Get-Date` cmdlet documentation: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- `[DateTime]::ParseExact` method documentation: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact
- Regular expressions in PowerShell: https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-regexes
---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:37:52.936277-07:00
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string is about making sense of date information contained within text. Programmers do it to enable calculations, comparisons, and storing dates in a standardized format.

## How to:
PowerShell makes date parsing pretty straightforward. Let's look at how to transform a string into a DateTime object.

```PowerShell
# Basic parsing using ParseExact
$dateString = "March 31, 2023"
$format = "MMMM dd, yyyy"
$parsedDate = [datetime]::ParseExact($dateString, $format, $null)

# Output
$parsedDate
```

This will output:

```
Friday, March 31, 2023 12:00:00 AM
```

Sometimes, strings have different formats. Let’s tackle that with `Parse` default:

```PowerShell
# Parse a date with different formats
$dateString = "2023-03-31T14:45:00"
$parsedDate = [datetime]::Parse($dateString)

# Output
$parsedDate
```

The output here:

```
Friday, March 31, 2023 2:45:00 PM
```

Dealing with culture-specific formats? Use `ParseExact` with a specific culture:

```PowerShell
# Culture-specific parsing
$dateString = "31/03/2023 16:45"
$format = "dd/MM/yyyy HH:mm"
$culture = [Globalization.CultureInfo]::GetCultureInfo("en-GB")
$parsedDate = [datetime]::ParseExact($dateString, $format, $culture)

# Output
$parsedDate
```

Which outputs:

```
Friday, March 31, 2023 4:45:00 PM
```

## Deep Dive
Now for some nitty-gritty. Date parsing has always been fickle due to various global date formats. Way back, each programming language had its own way of handling dates, often leading to inconsistencies. PowerShell, being a newer shell script language, benefited from .NET's DateTime class, providing robust parsing functions.

For alternatives, we've got `Get-Date`, which can parse a string into a DateTime object too. Consider `TryParseExact` and `TryParse` if you expect the unexpected and want to avoid exceptions from unparseable strings.

Let's talk implementation. Date parsing is culture-sensitive, meaning the date's format may vary per region. That's why we provide culture and format information to nail down the expected structure of the date string.

Sometimes you'll run into very quirky formats—enter the `ParseExact` method, your Swiss Army knife for date parsing. It lets you specify the exact format you're expecting. No guessing games here.

## See Also
To expand your knowledge, dig into these resources:

- [PowerShell official documentation on Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET documentation on DateTime.Parse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-6.0)
- [Globalization and localization considerations](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)

Remember, date parsing is powerful, so wield it wisely!

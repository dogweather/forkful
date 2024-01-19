---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string involves extracting date information from text and transforming it into a date/time format that your system can understand. Programmers do this as it permits easier manipulation and comparison of the extracted dates.

## How to:

PowerShell comes with a straightforward cmdlet `[datetime]::ParseExact()` for parsing dates from strings. Let's dive in:

```PowerShell
# Define the date string and the expected format
$dateString = '05-21-2021'
$format = 'MM-dd-yyyy'
# Parse the date
$date = [datetime]::ParseExact($dateString,$format,$null)
# Print the date
Write-Host $date
```

Output:
```
2021-05-21 00:00:00
```
>Note: The date could perform an error if the string doesn't match the expected format!

## Deep Dive

1. **Historical Context:** Date parsing has been in PowerShell since its early versions (beginning in 2006). The method has been improved over time for better speed and versatility. 

2. **Alternatives:** There are numerous other ways to parse a date from a string in PowerShell, such as using `Get-Date` cmdlet.

   ```PowerShell
   $dateString = "2021-05-21"
   $date = Get-Date -Date $dateString
   Write-Host $date
   ```

   Output:
   ```
   2021-05-21 00:00:00
   ```

3. **Implementation Details:** Parsing varies by the parsed format's string locale/culture. In our example, we used $null for the culture parameter, which defaults to the current culture info.

## See Also:

1. [ParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)
2. [PowerShell Get-Date Cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
3. [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0) for culture specific parsing.
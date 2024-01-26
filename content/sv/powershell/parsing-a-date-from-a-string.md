---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:37:57.040485-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att tolka ett datum från en sträng innebär att omvandla text till ett datumformat som PowerShell förstår. Programmerare gör detta för att enkelt hantera och manipulera datum relaterat data, exempelvis inom logganalys eller användardata.

## How to:
```PowerShell
# Parsing a date from a string with the ParseExact method
$dateString = "2023-04-05"
$format = "yyyy-MM-dd"
$parsedDate = [datetime]::ParseExact($dateString, $format, $null)

# Displaying the result
$parsedDate
```
```
onsdag 5 april 2023 00:00:00
```

## Deep Dive
Parsing av datumsträngar handlar om att konvertera text till ett 'DateTime'-objekt. Innan .NET och PowerShell blev vanligt, användes komplicerade script och tredjepartsverktyg för detta ändamål. I PowerShell, tack vare dess .NET-underliggande, är det mycket mer rakt på sak. `ParseExact`-metoden tillåter exakt specificering av datumformatet vilket minskar missförstånd som kan uppstå med olika kulturella datumformat. Alternativ som `Parse` och `TryParse` finns också; de är mer förlåtande om formatet varierar men kan leda till fel om strängen inte är väldefinierad. Implementationsdetaljer inkluderar hantering av olika kalendrar och tidzoner som kan konfigureras via det tredje argumentet i `ParseExact`, som är en `IFormatProvider`.

## See Also
- [.NET's DateTime structure reference](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [CultureInfo Class: Handling of different cultures in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)

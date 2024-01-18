---
title:                "Uppdelning av en datum från en sträng"
html_title:           "PowerShell: Uppdelning av en datum från en sträng"
simple_title:         "Uppdelning av en datum från en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en datumsträng till ett datum är en vanlig uppgift för utvecklare. Det kan till exempel vara användbart när man vill sortera data efter datum eller jämföra olika datumvärden. Det finns inga inbyggda funktioner i PowerShell som direkt konverterar en datumsträng, men det går att använda vissa metoder för att åstadkomma detta.

## Hur?
Här är ett exempel på hur man kan konvertera en datumsträng till ett datum i PowerShell:

```PowerShell
[DateTime]::ParseExact("20200501", "yyyyMMdd", $null)
```

Det här koden kommer att skapa ett DateTime-objekt med värdet 2020-05-01 baserat på den givna datumsträngen och det specifierade formatet.

En annan metod är att använda en typomvandlare för att konvertera datatypen från sträng till datum:

```PowerShell
[DateTime]"20200501"
```

Det här kommer också att returnera ett DateTime-objekt med värdet 2020-05-01.

## Djupdykning
I äldre versioner av PowerShell kunde man använda metoden [System.DateTime]::Parse() för att konvertera en datumsträng till ett datum. Denna metod har sedan ersatts av [DateTime]::ParseExact() som ger en mer preciserad kontroll över hur datumet tolkas.

Det finns också andra sätt att konvertera en datumsträng, som att använda Get-Date cmdlet eller använda .NET-klassen [System.Convert]. För mer komplexa parsinguppgifter kan man använda Regular Expressions.

## Se även
- [System.DateTime]::ParseExact() Dokumentation: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact
- Typomvandlare Dokumentation: https://docs.microsoft.com/en-us/powershell/scripting/learn/cookbooks/using-data-types?view=powershell-7
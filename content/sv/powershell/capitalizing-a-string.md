---
title:                "Gör om en sträng till versaler"
html_title:           "PowerShell: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng innebär att ändra alla dess tecken till stora bokstäver. Programmerare gör detta för standardisering och för att göra sträng jämförelser oberoende av skiftläge.

## Hur man gör:

Här kommer vi att se hur vi kapitaliserar en sträng i PowerShell med hjälp av metoden `ToUpper()`:

```PowerShell
$str = 'hej världen'
$capitalizedStr = $str.ToUpper()
Write-Output $capitalizedStr
```

När du kör ovanstående skript blir utdata:

```PowerShell
HEJ VÄRLDEN
``` 

## Djup Dykning:

Historiskt sett har datorprogram skrivits för att vara skiftkänsliga som ett sätt att spara på processorkraft och minne. Kapitalisering av strängar blev ett vanligt sätt att förhindra problem med detta. 

Som ett alternativ till `ToUpper()` kan du använda `.ToString().ToUpperInvariant()` om du vill göra konverteringen oberoende av tidsinställningar och kultur. 

Vid implementering, `ToUpper()`-metoden i PowerShell använder .NET ramverket under huven för att konvertera alla tecken i en sträng till stora bokstäver. 

## Se Också:

- [Microsoft .NET Documentation - ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Microsoft PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [String Capitalization Methods in PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/string-formatting-with-powershell/)
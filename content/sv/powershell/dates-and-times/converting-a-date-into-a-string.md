---
date: 2024-01-20 17:37:08.458525-07:00
description: "S\xE5h\xE4r g\xF6r du: Konvertera ett datum till en str\xE4ng."
lastmod: '2024-03-13T22:44:38.137286-06:00'
model: gpt-4-1106-preview
summary: "Konvertera ett datum till en str\xE4ng."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Såhär gör du:
Konvertera ett datum till en sträng:

```PowerShell
$datum = Get-Date
$datumSträng = $datum.ToString("yyyy-MM-dd")
Write-Output $datumSträng
```

Utdataexempel:

```
2023-04-12
```

Anpassa formatet:

```PowerShell
$anpassatDatum = $datum.ToString("dd/MM/yyyy HH:mm")
Write-Output $anpassatDatum
```

Utdataexempel:

```
12/04/2023 14:30
```

## Djupdykning:
I PowerShell använder `.ToString()` metoden .NET:s inbyggda datatyper och metoder för datum och strängar. Traditionellt i programmering, från tidiga dagar av C och Unix, har datum hanterats via strukturer och funktioner för formatering. PowerShell erbjuder också cmdleten `Get-Date` för att skapa och manipulera datumobjekt och metoderna `ToString()` och `Format()` för att omvandla dessa objekt till strängar.

Alternativt kan man använda standardformatet:

```PowerShell
$standardDatumSträng = Get-Date -Format "yyyy-MM-dd"
Write-Output $standardDatumSträng
```

Detta utför samma sak som `ToString()` exempel ovan men med en kortare syntax. PowerShell stöder en mängd fördefinierade format samt möjligheten att definiera egna.

## Se också:
- Microsofts dokumentation om `Get-Date`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date
- Översikt över standard .NET datum- och tidsformatsträngar: https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings
- Anpassade .NET datum- och tidsformatsträngar: https://docs.microsoft.com/dotnet/standard/base-types/custom-date-and-time-format-strings

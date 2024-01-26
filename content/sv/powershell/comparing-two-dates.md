---
title:                "Jämföra två datum"
date:                  2024-01-20T17:33:31.466135-07:00
model:                 gpt-4-1106-preview
simple_title:         "Jämföra två datum"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämföra två datum handlar om att avgöra vilket datum som kommer först, eller om de är samma. Programmerare gör detta för att hantera tidsfrister, schemalägga händelser eller spåra tidsskillnader.

## Hur man gör:
PowerShell gör det enkelt. Använd `Get-Date` för att skapa datumobjekt och jämför dem med `-lt` (mindre än), `-gt` (större än) eller `-eq` (lika med).

```PowerShell
$datum1 = Get-Date '2023-04-01'
$datum2 = Get-Date '2023-04-15'

# Är datum1 tidigare än datum2?
$datum1 -lt $datum2  # Ger Tillbaka True

# Är datum2 senare än datum1?
$datum2 -gt $datum1  # Ger Tillbaka True

# Är de samma datum?
$datum1 -eq $datum2  # Ger Tillbaka False
```

Om du vill räkna ut skillnaden mellan två datum:

```PowerShell
$skillnad = $datum2 - $datum1
$skillnad.Days  # Visar antal dagar mellan datum
```

## Djupdykning
Datumbearbetning är grundläggande i datorsystem sedan deras urmodiga dagar. PowerShell använder .NET's `DateTime` objekt, vilket gör det robust och flexibelt. Alternativ för att jämföra datum inkluderar användning av `Compare-Object` eller `[datetime]::Compare` metoderna, men direkt jämförelse med operatörer är ofta enklare.

Om du jobbar med tidszoner, överväg `[DateTimeOffset]` för mer noggrannhet. Prestandan är viktig i stora datummängder, så håll jämförelserna så enkla som möjligt. För historiska datum, PowerShell hanterar datum så långt tillbaka som år 0001.

## Se även
- Microsofts dokumentation om `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- Mer om [DateTime]-objekt: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Lär dig om [DateTimeOffset]: https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset

---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:21.038391-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet innebär att få fram dagens datum och tid. Programmerare gör detta för att logga händelser, hantera kalenderfunktioner eller helt enkelt för att ge användare information om när något skedde.

## Hur gör man:
För att få fram aktuellt datum och tid i PowerShell, använd `Get-Date` cmdleten. Här är några användningsexempel:

```PowerShell
# Hämta aktuellt datum och tid
Get-Date

# Formatera utdata till en specifik sträng
Get-Date -Format "yyyy-MM-dd HH:mm:ss"

# Skapa en datumsträng för filnamn
Get-Date -Format "yyyyMMdd_HHmmss"
```

Exempel på utmatning:

```PowerShell
# Utmatning av 'Get-Date'
torsdag 23 mars 2023 14:45:12

# Utmatning för formaterad sträng
2023-03-23 14:45:12

# Datumsträng för filnamn
20230323_144512
```

## Fördjupning
`Get-Date` cmdleten har funnits sedan PowerShell introducerades, vilket ger en enkel väg till systemets klocka och kalender. Alternativa sätt att hantera datum inkluderar .NET-klasser som `[System.DateTime]`, vilket kan erbjuda ytterligare funktionalitet vid behov. Implementationsdetaljer för `Get-Date` involverar hur PowerShell interagerar med systemets tidsinställningar och konverterar det till olika format, samt hantering av tidszoner.

## Se även
- Microsofts officiella dokumentation för `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- Översikt över `[System.DateTime]` klassen: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Datum och tid formatering i PowerShell: https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-date-and-time?view=powershell-7.2

Kom ihåg att du kan leka runt med dessa kommandon och utforska mer om du vill göra mer komplexa saker med datum och tider i dina skript!

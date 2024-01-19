---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Få tag på dagens datum i PowerShell: En snabbguide

## Vad & Varför?
Att få det aktuella datumet innebär bara att hämta systemets inbyggda "nuvarande" datum och tid. Detta är användbart när du vill logga händelser, tidsstämpla filer, eller beräkna intervall.

## Hur till:
För att få det nuvarande datumet och tiden i PowerShell, använd kommandot `Get-Date`.

```PowerShell
Get-Date
```

Exempel på output:

```PowerShell
onsdag 4 augusti 2021 12:00:00
```

För att få dagens datum utan tiden, använd kommandot `Get-Date -DisplayHint Date`.

```PowerShell
Get-Date -DisplayHint Date
```

Exempel på output:

```PowerShell
onsdag 4 augusti 2021
```

## Fördjupning
`Get-Date` är en del av .NET Framework och har funnits sedan PowerShell v1.0. Det finns alternativ, till exempel `[datetime]::Now` och `[datetime]::Today`, men `Get-Date` är mer lättanvänt och ger mer flexibilitet. Under huven ringer `Get-Date` `DateTime.Now`-metoden, som i sig ringer systemets API för att få den aktuella datumen och tiden.

## Se även:
1. PowerShell dokumentation om `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
2. .NET Framework DateTime dokumentation: https://docs.microsoft.com/dotnet/api/system.datetime
3. Jämförelse mellan `Get-Date` och `[datetime]::Now`: https://adamtheautomator.com/get-date-powershell/
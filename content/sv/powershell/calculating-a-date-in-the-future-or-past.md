---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
aliases:
- sv/powershell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:55.303881-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett framtida eller förflutet datum innebär att addera eller subtrahera tid från en specifik datumtidpunkt. Programmerare gör detta för att hantera uppgifter som tidsgränser, tidsplanering och loggning.

## Hur gör man:
```PowerShell
# Beräknar ett datum 10 dagar framåt
$datumIdag = Get-Date
$framtid = $datumIdag.AddDays(10)
Write-Output $framtid

# Beräknar ett datum 20 dagar bakåt
$fortid = $datumIdag.AddDays(-20)
Write-Output $fortid
```
Exempeloutput:
```
torsdag den 20 april 2023 14:22:48
tisdag den 21 mars 2023 14:22:48
```

## Djupdykning
Tidigare ansågs tidshantering i programmering som en komplex uppgift, speciellt över olika tidszoner och skottår. PowerShell förenklar detta med inbyggda funktioner som `AddDays`. Andra språk erbjuder liknande funktioner, som `datetime` i Python eller `Date` i JavaScript. PowerShell använder .NET:s `DateTime` objekt för dessa operationer, som har hög precision och stöd för olika kalendrar. 

För utökad funktionalitet, som komplexa tidszonsberäkningar eller historisk tid, kan programmerare använda ytterligare moduler som .NET's `TimeZoneInfo` eller öppen källkodsprojekt som NodaTime.

## Se Även
- Microsoft's officiella dokumentation för [`Get-Date`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
- [`DateTime` strukt dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [NodaTime projekt](https://nodatime.org/) för komplex tidshantering
- [PowerShell Gallery](https://www.powershellgallery.com/) för att hitta användbara moduler och skript från communityn

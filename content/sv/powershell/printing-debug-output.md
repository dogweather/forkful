---
title:                "Utskrift av Felsökningsutdata"
html_title:           "PowerShell: Utskrift av Felsökningsutdata"
simple_title:         "Utskrift av Felsökningsutdata"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut felsökningsinformation är ett sätt för programmerare att få en bättre förståelse för hur deras kod fungerar och för att identifiera eventuella fel eller buggar. Det är också ett bra sätt att spåra variabler eller data som programmet behandlar under körning.

## Hur man gör:
En enkel metod för att skriva ut felsökningsinformation är genom att använda Write-Host cmdlet i PowerShell. Detta låter oss skriva ut variabler, strängar eller andra typer av data till konsolen.

```powershell
$x = 5
Write-Host "värdet av x är $x."
```
Output: Värdet av x är 5.

För att se utskriften från Write-Host cmdlet i grön text, kan vi använda -ForegroundColor parameter tillsammans med värdet "Green".

```powershell
$x = 5
Write-Host "värdet av x är $x." -ForegroundColor Green
```
Output: Värdet av x är 5 i grön text.

## Djupdykning:
Skrivande av felsökningsinformation har funnits sedan de tidiga dagarna av programmering, när man använde utskrivna utskrifter som ett sätt att spåra programmet. Det har blivit mer standardiserat och konsoliderat i moderna programmeringsspråk, vilket inkluderar PowerShell.

Det finns också andra sätt att skriva ut felsökningsinformation, som att använda Write-Debug cmdlet. Skillnaden mellan Write-Host och Write-Debug är att Write-Debug endast skrivs ut om felsökning är aktiverat för skriptet eller kommandot.

För att aktivera felsökning för ett PowerShell skript, kan du använda -Debug flaggan när du kör skriptet från PowerShell prompten.

Ytterligare ett alternativ för att skriva ut felsökningsinformation är att använda Start-Transcript cmdlet, vilket sparar all konsolutmatning till en loggfil. Detta är särskilt användbart om du vill ha en fullständig spårningslogg av allt skriptet gör.

## Se också:
- [Officiell dokumentation för Write-Host cmdlet](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.utility/write-host?view=powershell-7)
- [Officiell dokumentation för Write-Debug cmdlet](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7)
- [Officiell dokumentation för Start-Transcript cmdlet](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.host/start-transcript?view=powershell-7)
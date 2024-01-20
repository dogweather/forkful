---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

# PowerShell: Skriva ut Debug Output

## Vad & Varför?

Skriva ut debug output är processen att visa kodutförande data till utvecklaren. Det används av programmerare för att hitta och lösa problem i koden.

## Hur man gör:

Använd `Write-Debug` cmdlet för att skriva ut debug information. Kontrollera att du har aktiverat debug-preferenserna genom `Set-PSDebug` -Verbose.

```PowerShell
# Aktivera debug output
Set-PSDebug -Trace 2

# Skriv ut debug output
Write-Debug "Det här är en debug-meddelande"
```
Du borde se:
```PowerShell
DEBUG: Det här är en debug-meddelande
```
## Fördjupning

PowerShell förbättrade debuggingmöjligheter med introduktionen av `Set-PSDebug` cmdlet i version 5.0 som ett alternativ till traditionell `Write-Host` eller `Write-Output`.

`Write-Debug` erbjuder en mer kontrollerad utgång bygger på debug preferensvärde jämfört med `Write-Host` eller `Write-Output` som skriver ut output direkt till konsolen.

Implementationdetaljer av debugutskrift i PowerShell är att den är kopplad till debug-strömmen och inte till standardoutputströmmen, vilket möjliggör att filtrera och omdirigera output för bättre hantering.

## Se också

För mer information om debugging i PowerShell, se följande länkar:
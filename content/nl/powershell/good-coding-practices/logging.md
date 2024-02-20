---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:45.905587-07:00
description: "Loggen is in feite een broodkruimelspoor achterlaten door je code -\
  \ het is hoe je bijhoudt wat er gebeurt wanneer je script in het wild draait.\u2026"
lastmod: 2024-02-19 22:05:10.120245
model: gpt-4-0125-preview
summary: "Loggen is in feite een broodkruimelspoor achterlaten door je code - het\
  \ is hoe je bijhoudt wat er gebeurt wanneer je script in het wild draait.\u2026"
title: Logboekregistratie
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in feite een broodkruimelspoor achterlaten door je code - het is hoe je bijhoudt wat er gebeurt wanneer je script in het wild draait. Programmeurs loggen om te debuggen, om app-gedrag te volgen, om de prestaties te monitoren en om uit te kijken voor eventuele ongein.

## Hoe te:
Hier is de basisinformatie over het toevoegen van enkele basislogboekregistraties in je scripts:

```PowerShell
# Een eenvoudige logboodschap maken
Write-Host "Info: Starten van het scriptproces."

# Naar een bestand schrijven
"Info: Dit is een gelogd bericht." | Out-File -Append mijnLog.log

# Gebruikmaken van de ingebouwde cmdlet voor meer gedetailleerde logboekregistratie
Start-Transcript -Path "./gedetailleerdeLog.log"
Write-Output "Waarschuwing: Er klopt iets niet."
# ... je script doet dingen
Stop-Transcript

# Uitvoer van gedetailleerdeLog.log
******************************
Windows PowerShell transcript start
Starttijd: 20230324112347
Gebruikersnaam: PShellGuru@example.com
Uitvoeren als gebruiker: PShellGuru@example.com
Configuratienaam: 
Machine: PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Hosttoepassing: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Proces-ID: 2024
PS Versie: 7.1.2
```

Nu is er in je logboeken een verslag van wat je code heeft uitgevoerd.

## Diepgaand:
Historisch gezien is loggen bijna zo oud als programmeren zelf. Het is als een scheepsdagboek, maar dan voor software. Vroeger waren het misschien printouts of teletypes; nu draait alles om bestanden en geavanceerde logsbeheersystemen.

Wanneer je in de PowerShell-trenches zit, is `Write-Host` snel en vuil, maar het spuwt alleen tekst uit naar de console, niet geweldig voor het bijhouden van records. `Out-File` geeft je een eenvoudige manier om tekst in een bestand te gooien, maar voor het echte werk wil je `Start-Transcript` en `Stop-Transcript` gebruiken, die alles loggen - invoer, uitvoer, het complete plaatje.

Alternatieven? Zeker, als je op ondernemingsniveau bezig bent, kun je kijken naar Windows Event Log of software zoals Logstash gebruiken, maar voor je dagelijkse script houd je het best bij de tools van PowerShell. Wat implementatie betreft, onthoud om slim te loggen – te weinig en het is nutteloos, te veel en het is achtergrondruis.

## Zie Ook:
Bekijk deze om alles over loggen in PowerShell te begrijpen:

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.284376-07:00
description: "Hvordan: PowerShell tilbyr en enkel m\xE5te \xE5 sjekke for tilstedev\xE6\
  relsen av en mappe ved bruk av `Test-Path` cmdleten. Denne cmdleten returnerer en\
  \ Boolean-\u2026"
lastmod: '2024-03-13T22:44:41.031531-06:00'
model: gpt-4-0125-preview
summary: "PowerShell tilbyr en enkel m\xE5te \xE5 sjekke for tilstedev\xE6relsen av\
  \ en mappe ved bruk av `Test-Path` cmdleten."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
PowerShell tilbyr en enkel måte å sjekke for tilstedeværelsen av en mappe ved bruk av `Test-Path` cmdleten. Denne cmdleten returnerer en Boolean-verdi som indikerer om den spesifiserte stien finnes. Her er hvordan du kan bruke den:

```powershell
# Sjekk om en mappe finnes
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Finnes mappen? $directoryExists"
```

Eksempelutdata for en mappe som finnes:

```
Finnes mappen? True
```

Og for en mappe som ikke finnes:

```
Finnes mappen? False
```

For mer komplekse skript, spesielt de som samhandler med nettverksandeler eller nettsky-lagring, kan det hende du trenger ekstra sjekker eller funksjonalitet som ikke er direkte tilgjengelig gjennom `Test-Path`. I slike tilfeller kan det være nyttig å bruke tredjeparts PowerShell-moduler eller biblioteker, selv om de fleste rutinemessige oppgavene kan utføres med PowerShells innebygde cmdleter. Per min siste kunnskapsoppdatering, har det ikke vært et bredt akseptert tredjeparts bibliotek spesifikt for å sjekke eksistensen av mapper utover det `Test-Path` tilbyr, hovedsakelig fordi `Test-Path` i seg selv er både robust og effektivt for dette formålet.

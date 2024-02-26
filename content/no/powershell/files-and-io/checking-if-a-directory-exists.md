---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.284376-07:00
description: "I PowerShell er det \xE5 sjekke om en mappe finnes en vanlig oppgave\
  \ som hjelper skript \xE5 ta beslutninger basert p\xE5 filsystemets struktur\u2014\
  som \xE5 unng\xE5 feil\u2026"
lastmod: '2024-02-25T18:49:39.209709-07:00'
model: gpt-4-0125-preview
summary: "I PowerShell er det \xE5 sjekke om en mappe finnes en vanlig oppgave som\
  \ hjelper skript \xE5 ta beslutninger basert p\xE5 filsystemets struktur\u2014som\
  \ \xE5 unng\xE5 feil\u2026"
title: Sjekker om en mappe eksisterer
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I PowerShell er det å sjekke om en mappe finnes en vanlig oppgave som hjelper skript å ta beslutninger basert på filsystemets struktur—som å unngå feil ved å bekrefte at en målmappe er på plass før man forsøker å lese fra eller skrive til den. Det er essensielt for å sikre at skriptet ditt oppfører seg pålitelig i ulike miljøer.

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

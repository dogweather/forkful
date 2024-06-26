---
date: 2024-01-26 01:07:18.264099-07:00
description: "Hvordan gj\xF8re det: Her er det grunnleggende for \xE5 krydre skriptene\
  \ dine med enkel logging."
lastmod: '2024-03-13T22:44:41.023837-06:00'
model: gpt-4-1106-preview
summary: "Her er det grunnleggende for \xE5 krydre skriptene dine med enkel logging."
title: "Loggf\xF8ring"
weight: 17
---

## Hvordan gjøre det:
Her er det grunnleggende for å krydre skriptene dine med enkel logging:

```PowerShell
# Opprette en enkel loggmelding
Write-Host "Info: Starter skriptprosessen."

# Skrive til en fil
"Info: Dette er en logget melding." | Out-File -Append minLogg.log

# Bruk av innebygd cmdlet for mer detaljert logging
Start-Transcript -Path "./detaljertLogg.log"
Write-Output "Advarsel: Noe stemmer ikke helt."
# ... skriptet ditt gjør ting
Stop-Transcript

# Utdata av detaljertLogg.log
******************************
Windows PowerShell transkriptstart
Starttid: 20230324112347
Brukernavn  : PShellGuru@example.com
KjørSom Bruker: PShellGuru@example.com
Konfigurasjonsnavn: 
Maskin  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Vertsprogram: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Prosess-ID: 2024
PS-versjon: 7.1.2
```

Nå har du i loggene dine et spill for spill av hva koden din har vært opptatt med.

## Dypdykk:
Historisk sett er logging nesten like gammel som programmering selv. Det er som en kapteins logg, men for programvare. Tilbake i tiden, kunne det ha vært utskrifter eller teletypeskrivemaskiner; nå handler alt om filer og fancy logghåndteringssystemer.

Når du er nede i PowerShell-skyttergravene, er `Write-Host` raskt og skittent, men det spyttet bare ut tekst til konsollen, ikke flott for å holde poster. `Out-File` gir deg en enkel måte å slenge tekst inn i en fil, men for den virkelige saften vil du ha `Start-Transcript` og `Stop-Transcript` som logger alt—innput, output, hele sulamitten.

Alternativer? Klart, hvis du driver i stor skala, kan du se på Windows Event Log eller bruke programvare som Logstash, men for dag-til-dag skriptet, hold deg til PowerShell sine verktøy. Når det gjelder implementering, husk å logge smart – for lite og det er ubrukelig, for mye og det blir hvit støy.

## Se også:
Sjekk ut disse for å få tak på alt som omhandler logging i PowerShell:

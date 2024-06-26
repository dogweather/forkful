---
date: 2024-01-20 17:46:17.450478-07:00
description: "How to: (Hvordan:) Substrings har v\xE6rt del av tekstbehandling i programmering\
  \ s\xE5 lenge vi har h\xE5ndtert tekstdata. `Substring`-metoden finnes i mange\u2026"
lastmod: '2024-04-05T22:50:55.008264-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) Substrings har v\xE6rt del av tekstbehandling i programmering\
  \ s\xE5 lenge vi har h\xE5ndtert tekstdata."
title: Uthenting av delstrenger
weight: 6
---

## How to: (Hvordan:)
```PowerShell
# Eksempel 1: Bruk av 'Substring' metoden
$fullStreng = "PowerShell er kraftfullt"
$delStreng = $fullStreng.Substring(0, 10) # henter 'PowerShell'
Write-Output $delStreng

# Eksempel 2: Splitte en streng og ta ut deler
$setning = "PowerShell|er|kraftfullt"
$ord = $setning.Split('|')
Write-Output $ord[1] # viser 'er'

# Eksempel 3: Bruke 'regex' for å trekke ut deler
$tekst = "KundeID: 12345 - KjøpID: 98765"
$match = [regex]::Match($tekst, '(?<=KundeID: )\d+').Value
Write-Output $match # gir '12345'
```
Output:
```
PowerShell
er
12345
```

## Deep Dive (Dypdykk)
Substrings har vært del av tekstbehandling i programmering så lenge vi har håndtert tekstdata. `Substring`-metoden finnes i mange språk; den lar oss hente ut en del av en streng basert på posisjoner. I PowerShell kan vi også bruke `Split`-metoden for å dele en streng etter et skilletegn og hente elementer fra resultatet. Regex (regular expressions) gir mer finjustert kontroll og kan virke voldsom, men er uunnværlig for komplekse tekstbehandlingsoppgaver. Det handler om å lære regex-syntaksen.

## See Also (Se Også)
- Microsoft dokumentasjon om String-metoder: https://docs.microsoft.com/en-us/dotnet/api/system.string
- Regex101 for å teste og forstå regex: https://regex101.com/
- PowerShell sin offisielle dokumentasjon: https://docs.microsoft.com/en-us/powershell/

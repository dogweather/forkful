---
date: 2024-01-20 17:46:17.450478-07:00
description: "\xC5 trekke ut understrenger betyr \xE5 plukke bestemte deler ut av\
  \ en tekststreng. Programmerere gj\xF8r dette for \xE5 analysere data, rense inndata\
  \ eller jobbe med\u2026"
lastmod: '2024-02-25T18:49:39.180744-07:00'
model: gpt-4-1106-preview
summary: "\xC5 trekke ut understrenger betyr \xE5 plukke bestemte deler ut av en tekststreng.\
  \ Programmerere gj\xF8r dette for \xE5 analysere data, rense inndata eller jobbe\
  \ med\u2026"
title: Uthenting av delstrenger
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å trekke ut understrenger betyr å plukke bestemte deler ut av en tekststreng. Programmerere gjør dette for å analysere data, rense inndata eller jobbe med specifikke tekstsegmenter.

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

---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:53.109763-07:00
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dato betyr å få tak i dagens dato og tid. Programmerere gjør dette for å loggføre hendelser, merke data og håndtere tidsavhengige funksjoner.

## Hvordan:
```PowerShell
# Få den nåværende datoen og tiden
Get-Date

# Resultat
onsdag 29. mars 2023 15:42:07

# Tilpass formatet
Get-Date -Format "yyyy-MM-dd"
# Resultat
2023-03-29

# Vis kun dagens dato, uten tid
(Get-Date).Date

# Resultat
onsdag 29. mars 2023 00:00:00
```

## Dypdykk
`Get-Date` kommandoen i PowerShell kom til i de tidlige dagene av PowerShell, som en del av versjon 1.0. Den erstattet eldre metoder for tidsregistrering, som for eksempel bruk av `DateTime`-objektet i .NET. En alternativ måte å hente dato på kan være `[System.DateTime]::Now`, men `Get-Date` er mer Powershell-idiomatisk og leverer mer umiddelbare formateringsmuligheter. Når det kommer til implementasjonsdetaljer, tar `Get-Date` hensyn til tidssonen som er satt på systemet, noe som er viktig for tidsavhengige programmer.

## Se Også
- [PowerShell DateTime dokumentasjon](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)

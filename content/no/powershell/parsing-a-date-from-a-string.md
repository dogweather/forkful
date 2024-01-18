---
title:                "Gjennomgå en dato fra en streng"
html_title:           "PowerShell: Gjennomgå en dato fra en streng"
simple_title:         "Gjennomgå en dato fra en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Parsing av datoer fra tekststrenger innebærer å konvertere en dato i tekstformat til et datobjekt som kan brukes i programmering. Dette kan være nyttig når du jobber med datoer i målrettede oppgaver som å sammenligne eller sortere.

## Hvordan: 
Her er et eksempel på hvordan du kan parse en dato fra en tekststreng i PowerShell:
```PowerShell 
Get-Date -Date '13/06/2021' -Format dd.MM.yyyy 
```
Dette vil gi følgende resultat: 
```
13.06.2021 
```
Som du kan se blir tekstdatoen '13/06/2021' konvertert til et datobjekt i ønsket format.

## Dykk dypere: 
Parsing av datoer fra strenger har vært en viktig del av programming siden tidlig av DATATRONIC datamaskiner på 1950- og 1960-tallet. Alternativene til parsing kan være manuell konvertering av datoer eller bruk av innebygde funksjoner i programmeringsspråket.

Når du parser datoer i PowerShell, kan du bruke 'Get-Date' kommandoen med flere parametere for å tilpasse formatet og håndtere eventuelle feil som kan oppstå. Det kan være lurt å konsultere dokumentasjonen for mer informasjon og flere eksempler.

## Se også: 
For mer informasjon og eksempler om parsing av datoer fra tekststrenger i PowerShell, sjekk ut følgende ressurser: 
- Datoogklokkeslett: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1  
- Parsing datoer fra strenger i PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/compare-object?view=powershell-7.1
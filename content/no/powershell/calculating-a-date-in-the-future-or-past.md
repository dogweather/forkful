---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "PowerShell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er metoden for å legge til eller trekke fra dager, uker, måneder, osv. fra en bestemt dato. Programmerere gjør dette vanligvis for å planlegge prosjekter, beregne forfalldatoer eller spore hendelser over tid.

## Hvordan å:
Her er noen kodeeksempler på hvordan du kan beregne en dato i fremtiden eller fortiden med PowerShell.

```PowerShell
# Få dagens dato
$dato = Get-Date
Write-Output "Dagens dato: $dato"

# Beregn en dato 7 dager i fremtiden
$nyDato = $dato.AddDays(7)
Write-Output "Ny dato: $nyDato"

# Beregn en dato 30 dager i fortiden
$gammelDato = $dato.AddDays(-30)
Write-Output "Gammel dato: $gammelDato"
```
Når du kjører koden, vil du få noe output som ligner på dette:

```PowerShell
Dagens dato: 02/06/2022 11:22:55
Ny dato: 09/06/2022 11:22:55
Gammel dato: 03/05/2022 11:22:55
```
## Deep Dive
Historisk sett har datoberegning vært en kritisk del av programmering, spesielt i applikasjonsutvikling og databaseadministrasjon. I PowerShell bruker vi `AddDays()`-metoden for å legge til eller trekke fra dager fra en dato. Det samme kan gjøres for uker, måneder og år ved å bruke `AddWeeks()`, `AddMonths()` og `AddYears()` metoder.

Det finnes også alternative metoder for datoberegning, som å bruke Unix Timestamp eller DateTime-strukturen i .NET. Men PowerShell tilbyr en enklere og mer intuitiv måte å håndtere datoberegning på.

Datoobjekter i PowerShell er instanser av .NET `DateTime`-klassen, så du kan bruke alle metodene og egenskapene som er tilgjengelige i denne klassen når du arbeider med datoer i PowerShell.

## Se Også
For mer informasjon om dato-beregning i PowerShell, se følgende kilder:

- [Manipuler datoer i PowerShell med AddDays, AddHours, AddSeconds, osv.](https://www.powershellmagazine.com/2014/01/10/manipulate-dates-in-powershell-with-adddays-addhours-addseconds-etc/)
- [Arbeide med DateTime-objekter og tidssoner i PowerShell](https://www.computerperformance.co.uk/powershell/powershell_date_math/)
- [PowerShell DateTime - Objekt](https://www.advancedinstaller.com/user-guide/powershell-datetime-object.html)
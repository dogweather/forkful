---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente nåværende dato i programmering betyr å få informasjon om det eksakte tidspunktet nå i form av dato. Vi gjør dette for å loggføre tidspunkt for hendelser, generere tidspesifikke data, eller å måle tid mellom operasjoner.

## Hvordan gjøre det:
Her er noen Bash-kodeeksempler for å vise hvordan du henter den nåværende datoen:

```Bash
# Få dagens dato
dato=$(date)
echo "Dagens dato er: $dato"

# Få dato i bestemt format
formatertDato=$(date '+%d-%m-%Y')
echo "Dagens dato i DD-MM-YYYY format: $formatertDato"
```
Typisk utdata:

```Bash
Dagens dato er: Tue Mar 9 14:34:51 CET 2021
Dagens dato i DD-MM-YYYY format: 09-03-2021
```
## Dypere Dykk
Å hente nåværende dato i programmering ble brukt i de første operative systemene. Unix, mor til Linux, introduserte `date` kommando på 70-tallet.

Det er forskjellige metoder for å hente datoen, avhengig av programmeringsspråket. For eksempel, i Python, vil vi bruke datetime biblioteket (`import datetime; datetime.datetime.now()`). 

Når Bash tolker din `date` kommando, kaller den opp en systemfunksjon som heter `gettimeofday`(eller lignende), som tar språkets interne tidsstruktur, og gir den tilbake fylt med nåværende tid.

## Se Også
- [`date` Man Page](https://linux.die.net/man/1/date): Komplett beskrivelse av `date` kommandoen og dens alternativer.
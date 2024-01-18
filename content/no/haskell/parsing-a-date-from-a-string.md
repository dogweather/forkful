---
title:                "Å analysere en dato fra en streng"
html_title:           "Haskell: Å analysere en dato fra en streng"
simple_title:         "Å analysere en dato fra en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av datoer fra strenger er en vanlig oppgave for programmører når de jobber med dato- og tidsrelaterte data. Det er prosessen med å konvertere en streng som representerer en dato til et datatypen som kan brukes i programmet. Dette er viktig for å kunne behandle og sammenligne datoer på en effektiv måte.

## Slik gjør du det:

```Haskell
import Data.Time.Format

-- Definerer en streng med en dato
datoStreng = "01.01.2020"

-- Bruker parseTimeOrError funksjonen og formateringsstreng for å parsere datoen fra strengen
dato = parseTimeOrError True defaultTimeLocale "%d.%m.%Y" datoStreng

-- Printer ut datoen i ønsket format
print $ formatTime defaultTimeLocale "%A, %d. %B %Y" dato

-- Output: Wednesday, 01. January 2020
```

## Dypdykk:

Parsing av data har vært en viktig del av programmering siden de tidlige dagene av datamaskiner. I dag finnes det mange alternative biblioteker og metoder for å gjøre dette, men Haskell's `Data.Time.Format` bibliotek er en pålitelig og enkel måte å gjøre dette på.

Noen programmeringsspråk har også innebygd støtte for parsing av datoer, men det er viktig å merke seg at denne støtten kan være forskjellig fra språk til språk og kan føre til uventede resultater. Derfor er det viktig å bruke et pålitelig bibliotek som `Data.Time.Format` for å sikre nøyaktig parsing av datoer.

## Se også:

- [Et nybegynnerkurs i Haskell](https://haskell.org/learn/) for å lære mer om programmets syntaks og funksjonaliteter.
- [Offisiell dokumentasjon](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html) for `Data.Time.Format` biblioteket.
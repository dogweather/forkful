---
title:                "Beregning av dato i fremtiden eller fortiden"
html_title:           "Haskell: Beregning av dato i fremtiden eller fortiden"
simple_title:         "Beregning av dato i fremtiden eller fortiden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmere. Dette kan være nyttig for å finne ut når noe skal skje, for eksempel å planlegge en hendelse eller vise tidsbestemte data.

## Hvordan:
Det finnes mange måter å utføre dato beregninger i Haskell, men en enkel metode er å bruke "Data.Time" modulen. Her er et eksempel på hvordan du kan beregne en dato 30 dager i fremtiden:

```Haskell
import Data.Time
import Data.Time.Calendar.OrdinalDate

addDays 30 $ fromGregorian 2019 03 31 
-- output: 2019-05-01
```

Her legger vi 30 dager til den nåværende datoen (31. mars 2019), ved hjelp av funksjonen "addDays" og "fromGregorian" som konverterer året, måneden og dagen til en "Day" datatype. Vi får da ut en ny dato (1. mai 2019).

## Dypdykk:
Å beregne datoer har vært en viktig oppgave innen programmering siden begynnelsen. Tidligere brukte man komplekse matematiske formler for å beregne datoer, men nå finnes det enkle og effektive funksjoner som gjør jobben enklere. Alternativt kan man også bruke tredjepartsbiblioteker som "time" eller "chronos" for å utføre dato beregninger. Implementasjonen av slike funksjoner er vanligvis basert på algoritmer og kalenderinformasjon.

## Se også:
- ["Data.Time" dokumentsjon](https://hackage.haskell.org/package/time)
- ["chronos" bibliotek](https://hackage.haskell.org/package/chronos)
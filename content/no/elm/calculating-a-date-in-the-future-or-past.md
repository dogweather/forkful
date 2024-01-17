---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Elm: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden handler om å bestemme en spesifikk dato basert på en startdato og et antall dager i tillegg eller substraksjon. Dette verktøyet er nyttig for programmerere for å håndtere tid og datoer i applikasjoner, for eksempel å planlegge fremtidige hendelser eller beregne aldre.

## Slik gjør du:
For å beregne en dato i fremtiden eller fortiden i Elm, kan du bruke funksjonen `addDays` fra `Time`-modulen. Denne funksjonen tar inn et heltall (dager) og en dato, og returnerer en ny dato som er det angitte antallet dager unna den opprinnelige datoen. Her er et eksempel som viser hvordan du kan legge til 30 dager til en dato:

```Elm
import Time exposing (..)
import Time.Extra exposing (..)

addDays 30 (fromDate 2022 Jan 1)
```

Resultatet vil være 2022 Jan 31. På samme måte kan du trekke fra dager ved å bruke et negativt heltall i `addDays`-funksjonen.

## Dykk dypere:
Historisk sett ble datoberegning gjort manuelt ved hjelp av kalendere og matematiske formler. I dag tilbyr de fleste programmeringsspråk, inkludert Elm, innebygde funksjoner for å beregne datoer for å gjøre det enklere for programmerere.

En alternativ måte å beregne datoer på i Elm er å bruke `addTime`-funksjonen som tar inn en `Time`-verdi i stedet for en dato. Implementeringsmessig fungerer `addDays`-funksjonen ved å konvertere datoen til millisekunder og legge til eller trekke fra antall millisekunder til den endelige dato. Det er også verdt å merke seg at datoberegning kan være mer kompleks når du tar hensyn til faktorer som skuddår og ulike månedslengder.

## Se også:
- Elm `Time`-modulen dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/
- Elm `Time.Extra`-modulen dokumentasjon: https://package.elm-lang.org/packages/elm/time/1.0.0/Time-Extra
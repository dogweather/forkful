---
title:                "Elm: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig i mange tilfeller. Det kan hjelpe deg med å planlegge fremtidige hendelser eller få oversikt over fortidige datoer. I denne bloggposten vil vi utforske hvordan du kan bruke Elm til å beregne datoer.

## Hvordan

For å beregne en dato i Elm, kan du bruke funksjonen `Date.fromParts` som tar inn år, måned og dag som parametere. Du kan også bruke funksjonen `Date.fromString` for å konvertere en tekststreng til en dato. Her er et eksempel på å beregne datoen 5. desember 2020:

```elm
import Date

Date.fromParts 2020 12 5
--> Ok (Dec 5 2020)

Date.fromString "2020-12-05"
--> Ok (Dec 5 2020)
```

Du kan også bruke funksjoner som `Date.add` og `Date.subtract` for å legge til eller trekke fra dager, måneder eller år til en dato. Her er et eksempel på å beregne datoen 100 dager fra nå:

```elm
import Date exposing (Day, Month, Year)

Date.add (1, Day) 100 (Date.fromParts 2020 12 5)
--> Ok (Dec 5 2020)

Date.add (1, Month) 100 (Date.fromParts 2020 12 5)
--> Ok (Mar 5 2021)

Date.add (1, Year) 100 (Date.fromParts 2020 12 5)
--> Ok (Dec 5 2100)
```

## Deep Dive

Når du arbeider med datoer i Elm, må du være oppmerksom på at datotyper som `Day`, `Month` og `Year` er enum-typer, som betyr at de har en begrenset og forhåndsdefinert mengde verdier. Dette kan føre til problemer hvis du for eksempel prøver å legge til 31 dager til en måned med 30 dager. I slike tilfeller vil Elm returnere en feil (Error).

For å unngå disse problemene, kan du bruke funksjonen `Date.addDays` som tar hensyn til antall dager i en måned. Det er også viktig å merke seg at Elm bruker den gregorianske kalenderen, som kan føre til uventet oppførsel når du arbeider med datoer før 1582.

## Se også

- Elm Dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Time pakken: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Gregorian pakken: https://package.elm-lang.org/packages/elm-community/elm-gregorian/latest/
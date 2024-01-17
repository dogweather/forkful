---
title:                "Sammenligning av to datoer"
html_title:           "Elm: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Å sammenligne to datoer" er en vanlig oppgave for programmere. Det innebærer å sjekke om to datoer er like, eller hvilken som kommer først, basert på en gitt format. Dette er nyttig for å sortere data og utføre ulike operasjoner på datoer.

## Hvordan:
I Elm er det enkelt å sammenligne to datoer ved hjelp av funksjonen `case` og det innebygde `Date` modulen. Her er et eksempel på hvordan du sammenligner to datoer for å se om de er like:

```Elm
import Date exposing (..)

date1 : Date
date1 = fromString "2019-05-30"

date2 : Date
date2 = fromString "2019-05-30"

case compare date1 date2 of
    LT -> "Dato 1 kommer før dato 2"
    EQ -> "Dato 1 og 2 er like"
    GT -> "Dato 2 kommer før dato 1"
```

Output:
`Dato 1 og Dato 2 er like`

## Deep Dive
Å sammenligne datoer har vært en vanlig oppgave i programmering siden datamaskiner ble først introdusert. I eldre språk som Cobol, måtte programmere håndtere datoer manuelt ved å konvertere de til numeriske verdier. Elm sin `Date` modul tar seg av dette for deg, og gjør sammenligning av datoer enkelt og nøyaktig.

Det finnes også alternative måter å sammenligne datoer på, som å bruke en tredjeparts bibliotek som håndterer datoer og tidspunkter. Men i Elm, er det anbefalt å bruke den innebygde `Date` modulen fordi den er designet spesifikt for å håndtere datoer og gir nøyaktige resultater.

Når du sammenligner to datoer, må du huske å bruke samme format for begge datoene for å få riktig resultat. For eksempel, hvis en dato er i formatet `YYYY-MM-DD`, må den andre også være i samme format.

## Se Også
Offisiell Elm dokumentasjon for `Date` modulen: https://package.elm-lang.org/packages/elm/time/latest/Date
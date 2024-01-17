---
title:                "Å få gjeldende dato"
html_title:           "Elm: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å få gjeldende dato refererer til å hente ut dagens dato og tidspunkt i et gjeldende tidsformat. Dette er nyttig for programmører for å kunne spore tidspunktet når koden ble kjørt eller for å oppdatere grensesnittet med riktig dato og tid.

## Hvordan:
```Elm
import Date exposing (Date, Time)

getDate: Date
getDate =
  Date.fromTimezone Time.utc

```

Utskrift:
```
2020-04-08 12:00:00 UTC
```

Her importeres ```Date``` biblioteket som gir tilgang til funksjoner for å håndtere dato og klokkeslett. Deretter brukes ```Date.fromTimezone``` funksjonen for å hente ut den gjeldende datoen og klokkeslettet i UTC format.

## Dykk ned:
1. Historisk kontekst:
Det har alltid vært viktig for programmerere å kunne spore og håndtere tid. Den første datoen var datert tilbake til Romerriket, men det var først på 1900-tallet at dato og klokkeslett ble standardisert internasjonalt.
2. Alternativer:
Det finnes mange måter å få gjeldende dato på, inkludert å bruke JavaScript for å hente ut dato på klientens side eller bruke et tredjeparts bibliotek som moment.js.
3. Implementasjonsdetaljer:
```Date``` biblioteket i Elm bruker en internt en numerisk representasjon av dato og klokkeslett som gjør det enkelt å håndtere dato og klokkeslett manipulasjoner.

## Se også:
- [Elm Date dokumentasjon](https://package.elm-lang.org/packages/elm-lang/core/1.0.5/Date)
- [Moment.js bibliotek](https://momentjs.com/)
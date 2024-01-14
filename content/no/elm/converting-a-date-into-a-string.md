---
title:    "Elm: Konvertere en dato til en streng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte nødvendig å konvertere en dato til en streng når man arbeider med programmering. Dette er spesielt viktig dersom man ønsker å vise datoen til brukeren på en leselig måte, for eksempel på et nettsted eller en applikasjon. I dette blogginnlegget vil vi utforske hvordan du kan konvertere en dato til en streng ved hjelp av Elm-programmeringsspråket.

## Hvordan gjøre det

Det første trinnet i å konvertere en dato til en streng er å bruke Elm sin innebygde funksjon `Date.toLocalString`. Denne funksjonen tar inn en dato og returnerer en formatering av datoen i henhold til brukerens lokale innstillinger. Her er et eksempel på hvordan du kan bruke denne funksjonen i din Elm-kode:

```Elm
import Date 

dateToString : Date.Date -> String
dateToString date =
    Date.toLocalString date |> Result.withDefault "Ugyldig dato"
```

I dette eksempelet bruker vi funksjonen `Date.toLocalString` til å konvertere `date` til en streng. Vi pakker også inn funksjonen i en `Result` for å håndtere eventuelle feil som kan oppstå under konverteringen.

Her er et eksempel på hvordan denne funksjonen kan brukes i praksis:

```Elm
date = Date.fromCalendarDate 2020 8 5
dateToString date
```

Dette vil returnere strengen "05.08.2020" i en norsk lokal innstilling.

## Dykk dypere

For de som ønsker å lære mer om hvordan Elm håndterer datoer og konverteringer, kan det være nyttig å se på hvordan funksjonen `Date.toLocalString` er implementert i kildelistedatabasen til Elm. Der kan man se at den bruker standardbibliotekets funksjon `Date.toLocaleString` og håndterer eventuelle feil som kan oppstå.

Man kan også eksperimentere med forskjellige innstillinger og formateringer ved å endre argumentene som sendes til `Date.toLocalString`.

## Se også

- `Date` modulen i Elm standardbiblioteket: https://package.elm-lang.org/packages/elm-lang/core/latest/Date
- Elm fellesskapets kilder til prosjekter og ressurser: https://elm-lang.org/resources
---
title:                "Elm: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være nyttig når du ønsker å filtrere data eller organisere informasjon etter dato. Det kan hjelpe deg å forstå når noe skjedde og få en bedre oversikt over tidsbaserte hendelser.

## Hvordan

For å sammenligne to datoer i Elm kan du bruke funksjonen `compare` fra [`Time`](https://package.elm-lang.org/packages/elm/time/latest/Time) pakken. Denne funksjonen tar to datoer som parametere og returnerer et `Comparison`-verdi som kan være `LT` (mindre enn), `EQ` (lik) eller `GT` (større enn). Her er et eksempel på hvordan du kunne sammenligne to datoer og deretter skrive ut resultatet:

```elm
import Time exposing (Date, Day, Month, Year, compare)
import Time.Format as Format

dato1 : Date
dato1 = Date.fromDate 2021 Jan 1

dato2 : Date
dato2 = Date.fromDate 2021 Jan 2

sammenligning : Comparison
sammenligning = compare dato1 dato2

output : String
output =
    "Dato 1 er " ++ case sammenligning of
        LT -> "mindre enn"
        EQ -> "lik"
        GT -> "større enn"
    ++ " dato 2."

main : Html msg
main =
    text (Format.format Date.rfc3339 dato1 ++ " " ++ output)
```

Dette eksemplet vil resultere i følgende utskrift:

```shell
2021-01-01T00:00:00+00:00 Dato 1 er mindre enn dato 2.
```

## Dypdykk

I tillegg til å bruke `compare` funksjonen, kan du også bruke `DateTime` modulen fra [`Time.Extra`](https://package.elm-lang.org/packages/elm/time-extra/latest/Time-Extra-DateTime) pakken for å utføre mer avanserte operasjoner med datoer. Denne modulen tilbyr funksjoner som `diffInDays` for å finne antall dager mellom to datoer, eller `addToTime` for å legge til et gitt antall dager eller måneder til en dato.

Det er også verdt å merke seg at Elm ikke har en egen dato og tidsstempel datatype, men i stedet bruker det `Date`, `Time` og `DateTime` verdityper fra [`Time`](https://package.elm-lang.org/packages/elm/time/latest/Time) pakken. Dette kan være noe å være oppmerksom på når du arbeider med datoer og klokkeslett i Elm.

## Se også

- [Elm Time package documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Time Extra package documentation](https://package.elm-lang.org/packages/elm/time-extra/latest/)
- [Elm Date and Time guide](https://guide.elm-lang.org/architecture/effects/time.html)
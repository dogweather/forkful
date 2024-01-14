---
title:                "Elm: Konvertering av en streng til små bokstaver"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Når vi jobber med tekstbehandling i programvare, kan det være nødvendig å konvertere strenger til små bokstaver. Dette kan være nyttig for å sammenligne strenger eller for en bedre brukeropplevelse. Elm har en innebygd funksjon for å gjøre dette, og i denne bloggposten skal vi se på hvordan den fungerer.

## Hvordan

For å konvertere en streng til små bokstaver i Elm, kan vi bruke funksjonen `String.toLower`. Her er et eksempel på hvordan det kan gjøres:

```Elm
import String

stringToConvert = "ELM Erin"
convertedString = String.toLower stringToConvert

```

Den første linjen importerer `String` modulen, som gir oss tilgang til `String.toLower` funksjonen. Deretter lager vi en variabel `stringToConvert` med en verdi. I neste linje bruker vi `String.toLower` til å konvertere variabelen til små bokstaver og lagrer den i en ny variabel `convertedString`. Når vi kjører koden, vil `convertedString` bli `"elm erin"`.

Dette er nyttig når vi for eksempel må sammenligne to strenger og vil ignorere forskjeller i store og små bokstaver. Vi kan også bruke dette til å formatere input fra brukeren til et konsistent format.

## Dykk dypere

Det er verdt å merke seg at `String.toLower` funksjonen kun konverterer bokstaver i det engelske alfabetet til små bokstaver. Eventuelle bokstaver med aksenter eller spesielle tegn vil forbli uendret. Det er også verdt å merke seg at funksjonen ikke endrer originalstrengen, men heller returnerer en ny konvertert versjon.

I tillegg til `String.toLower` finnes det også en tilsvarende `String.uppercase` funksjon for å konvertere til store bokstaver.

## Se også

- [Elm Docs - String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Docs - toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Elm Docs - uppercase](https://package.elm-lang.org/packages/elm/core/latest/String#uppercase)
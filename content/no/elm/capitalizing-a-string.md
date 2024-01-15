---
title:                "Stor bokstaver en streng."
html_title:           "Elm: Stor bokstaver en streng."
simple_title:         "Stor bokstaver en streng."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har noen gang jobbet med tekstbehandling i programmering, har du kanskje lurt på hvordan du kan gjøre enkle oppgaver som å gjøre det første bokstaven i et ord til en stor bokstav. Dette er hvor kapitalisering av en streng kommer inn. Å kunne kapitalisere en streng er en grunnleggende ferdighet som kan brukes i mange forskjellige programmeringsspråk, inkludert Elm.

## Hvordan gjøre det

Det er flere måter å kapitalisere en streng på i Elm. Her er et eksempel på en måte som bruker funksjonen `String.toUpper`:

```Elm
import String exposing (toUpper)

str = "dette er en streng"
kapitalisert = toUpper str

-- kapitalisert = "DETTE ER EN STRENG"
```

Her importeres `String` modulen, som inneholder funksjonen `toUpper` som tar imot en streng (i dette tilfellet `str`) som argument og returnerer en kapitalisert versjon av strengen i en ny variabel (i dette tilfellet `kapitalisert`).

En annen måte å kapitalisere en streng på er ved bruk av `String.toFirstUpper`:

```Elm
import String exposing (toFirstUpper)

str = "dette er en streng"
kapitalisert = toFirstUpper str

-- kapitalisert = "Dette er en streng"
```

Som du ser, blir bare den første bokstaven i hvert ord kapitalisert i stedet for hele strengen, noe som kan være nyttig i visse tilfeller.

## Dypdykk

I Elm er det også mulig å lage din egen funksjon for å kapitalisere en streng. Dette kan gjøres ved å bruke `String.foldl` for å iterere gjennom hvert tegn i strengen og bruke `String.fromChar` og `String.toUpper` for å endre tegnet til en stor bokstav. Her er et eksempel:

```Elm
import String exposing (fromChar, foldl, toUpper)

str = "dette er en streng"
kapitalisert = capitalize str

capitalize string =
    if String.length string == 0 then
        ""
    else
        let
            firstChar = String.left string 1
            restOfStr = String.dropLeft string 1
        in
        fromChar <| toUpper firstChar ++ foldl (\char s -> s ++ String.fromChar char) "" restOfStr

-- kapitalisert = "Dette Er En Streng"
```

Her kalles den våre egenlagde funksjonen `capitalize` og den tar imot strengen som argument. Funksjonen sjekker om strengen er tom, og hvis den ikke er det, så tar den først bokstaven, kapitaliserer den og legger den til resten av strengen ved å bruke en `foldl` funksjon. Det er flere måter å implementere en egen funksjon for kapitalisering av strenger i Elm, men denne er bare et eksempel på en mulig måte å gjøre det på.

## Se også

- [Elm dokumentasjon for String modul](https://package.elm-lang.org/packages/elm/core/latest/String)
- [En introduksjon til funksjonell programmering i Elm](https://medium.com/@fabio_nogueira/introduction-to-functional-programming-in-elm-f5022ec155f4)
- [Spillprogrammering i Elm](https://www.elm-tutorial.org/no/17-games.html)
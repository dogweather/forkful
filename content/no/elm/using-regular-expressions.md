---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk lar deg søke og manipulere tekst gjennom mønstergjenkjenning. De er et kraftfullt verktøy for programmerere for å håndtere komplekse tekstbehandlingsoppgaver raskt og effektivt.

## Hvordan:
Elm har ikke innebygd støtte for regulære uttrykk direkte, men du kan bruke `elm/regex` pakken for å jobbe med dem. Her er et enkelt eksempel på hvordan du finner e-postadresser i en tekststreng:

```Elm
import Regex exposing (..)

findEmails : String -> List String
findEmails text =
    let
        pattern = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
    in
    Regex.find (Regex.AtMost 1) (Regex.regex pattern) text
        |> List.map .match
```

Når du kjører `findEmails` med en tekststreng som argument, får du en liste av alle funnede e-postadresser.

## Dybdestudie
Regulære uttrykk har sin historie tilbake til automata-teori og formell språkteori på 1950-tallet. Alternativer til regulære uttrykk inkluderer string søkealgoritmer som KMP og tekstparsere som kan være mer effektive eller leselige avhengig av oppgaven.

I Elm håndteres regulære uttrykk gjennom `elm/regex` pakken, som kompilerer dine mønstre til JavaScripts regulære uttrykksmotor siden Elm kompilerer til JavaScript.

## Se Også
- Elm Regex Package: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Intro til Regulære Uttrykk: [regexone.com](https://regexone.com/)
- Elm Språk Guide: [guide.elm-lang.org](https://guide.elm-lang.org/)

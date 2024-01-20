---
title:                "Gjøre en streng stor"
html_title:           "Elm: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Setninger på kaptal er når det første tegnet i en streng (eller hvert ord) er et stort tegn. Programmerere gjør dette for å skape mer lesbare betingelser, eller å formaterer data for å matche visse regler.

## Hvordan Gjør Man Det:
Å bytte til store) bokstaver i Elm er rett fram. Du kan bruke `String.toUpper` funksjon

Her er et eksempel:

```Elm
import String

main =
  String.toUpper "dette er en setning."
```

Og utdata vil være:

`DETTE ER EN SETNING.`

For å bare sette den første bokstaven til stor kan du dele opp strengen og hente den første bokstaven, sette den til stor, og deretter sette sammen strengen igjen:

```Elm
import String

capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.toUpper (String.fromChar first) ++ rest

main =
  capitalize "dette er en setning."
```

Og utdata skal være:

`Dette er en setning.`

## Dypdykk
Det å sette tegn til stor bokstav i strenger er en viktig programmeringsfunksjon som har vært tilgjengelig i mange språk lenge. I eldre språk kan du ha måtte skrive din egen funksjon for å gjøre det. Elm tillater dette gjennom innebygde funksjoner som `String.toUpper` og `String.uncons`.

Det er verdt å merke seg at det finnes alternative metoder for å oppnå det samme målet. Du kan bruke regex (også tilgjengelig i andre språk) for å oppnå lignende resultater.

Implementeringsdetaljer handler om hvordan Elm håndterer strenger. Elm ser på strenger som lister av tegn, som gjør det lett å bruke funksjoner som `String.uncons` for å hente ut og manipulere enkelttegn.

## Se Også
Elms offisielle dokumentasjon gir mer informasjon om strenger og innebygde string-funksjoner som kan være nyttige:

- Elm String API: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- For dem som ønsker å dykke dypere inn i hvordan strenger håndteres i Elm: [https://guide.elm-lang.org/core_language.html](https://guide.elm-lang.org/core_language.html)
---
date: 2024-01-20 17:52:21.420264-07:00
description: "Debug-utskrift er \xE5 kaste ut data til konsollen for \xE5 spore hva\
  \ programmet ditt gj\xF8r. Programmerere gj\xF8r dette for \xE5 forst\xE5 feil og\
  \ forbedre kodeflyten."
lastmod: '2024-03-13T22:44:40.711540-06:00'
model: gpt-4-1106-preview
summary: "Debug-utskrift er \xE5 kaste ut data til konsollen for \xE5 spore hva programmet\
  \ ditt gj\xF8r."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## Hva & Hvorfor?
Debug-utskrift er å kaste ut data til konsollen for å spore hva programmet ditt gjør. Programmerere gjør dette for å forstå feil og forbedre kodeflyten.

## Hvordan:
I Elm, bruk `Debug.log` for å skrive ut vid debug-info i konsollen. Pass på, den er ment for lokalt arbeid – ikke for produksjon!

```Elm
import Html exposing (text)
import Debug

main =
    let
        _ = Debug.log "Inspected value" (4 * 3)
    in
    text "Se konsollen for debug-output!"
```

Du vil se dette i konsollen:

```
Inspected value: 12 : number
```

Denne koden multipliserer 4 med 3, og resultatet (12) vises i konsollen.

## Dypdykk:
`Debug.log` kom med Elm og har vært et nyttig verktøy siden. Alternativer til `Debug.log` inkluderer å lage egne logger-funksjoner eller bruk av eksterne verktøy som Elm Monitor. `Debug.log` tar to argumenter: en beskjed og verdien som skal inspiseres. Verdien blir returnert ubehandlet, så du kan smette `Debug.log` inn midt i uttrykk!

Husk at debug-utskrifter kan bremse applikasjonen og bør fjernes før produksjon for å beskytte sensitiv informasjon.

## Se Også:
- Elm's offisielle dokumentasjon om feilsøking: https://guide.elm-lang.org/effects/debugging.html
- Blogginnlegg om Elm og feilsøking: https://elm-lang.org/news/debugging-elm
- Elm Monitor for mer avansert debugging: https://github.com/layflags/elm-monitor

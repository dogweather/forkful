---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:13:36.263906-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Read-Eval-Print Loop (REPL) er et enkelt, interaktivt programmeringsmiljø som tar enkeltbrukerinput, evaluerer dem, og returnerer resultatet til brukeren. Elm-programmerere bruker REPL for raske eksperimenter, feilsøking eller for å lære språket.

## Hvordan:
Elm kommer ikke med en integrert REPL. Likevel kan du bruke `elm repl` fra kommandolinjen din for å starte en Elm økt etter å ha installert Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

I denne økten, etter å ha importert List-funksjoner, doblet vi tallene i en liste og fikk resultatet umiddelbart.

## Dypdykk
Elms REPL kan virke begrenset sammenlignet med de fra noen andre språk som Python eller JavaScript, siden Elm er et kompilert språk fokusert på å produsere webapplikasjoner. Historisk har Elm fokusert på hele applikasjoner snarere enn skripting eller skallinteraksjoner.

Alternativer til Elms REPL inkluderer `elm-live` og online redigeringsprogrammer som Ellie hvor du kan se endringer i koden reflektert i sanntid i en nettleser.

Når det gjelder implementasjon, kompilerer Elm REPL småstykker av Elm-kode til JavaScript i bakgrunnen, noe som tillater deg å kjøre Elm interaktivt. Dette er forskjellig fra REPL-er av tolkede språk, som ikke trenger dette kompileringssteget. Elm REPL er også nedstrippet for å holde kjerne språket lettvektig og fokusert.

## Se Også
- Elms offisielle guide om interaktivitet: https://guide.elm-lang.org/interop/
- Ellie, en online Elm-lekeplass: https://ellie-app.com/new
- `elm-live`, en fleksibel utviklingsserver for Elm: https://www.elm-live.com/

---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?

Att skriva ut felsökningsdata (debug output) är processen med att visa underliggande data och processflöde. Programmerare gör det för att spåra och diagnostisera oväntat beteende och fel i deras kod.

## Så här gör du:

```Elm
import Html exposing (Html, text)
import Debug

main =
  let
    myValue = "Hej världen!"
    _ = Debug.log "Mitt värde är" myValue
  in
  text myValue
```
Operationen `Debug.log` skriver ut följande text i webbläsarkonsolen: `Mitt värde är: "Hej världen!"`. Notera att `_ = Debug.log` är nödvändigt då `Debug.log` returnerar det värde som den just loggade och måste bindas.

## Djup Dykning

Historiskt sett, har utskrift av felsökningsdata varit ett grundläggande verktyg för programmerare ända sedan de första datorerna. Alternativ till Elm's `Debug.log` inkluderar användning av skräddarsydda funktioner för loggning eller användning av externa paket såsom Elm-Console. Men, vi behöver vara medvetna om att `Debug.log` endast ska användas under utveckling eftersom det tas bort från den slutliga byggen när man använder `--optimize` flaggan under byggprocessen.

## Se Även

- [Elm Guide: Debugging](https://guide.elm-lang.org/effects/): Fullständig guide på engelska till debuggan i Elm.
- [Elm: Beyond Hello World](https://guide.elm-lang.org/error_handling/): Avancerade koncept, inklusive felsökning och felhantering.

---
---
title:                "Å starte et nytt prosjekt"
html_title:           "Elm: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Starter et nytt prosjekt betyr å skape en ny eller videreutvikle en eksisterende programvare eller applikasjon. Dette gjøres vanligvis av-programmerere for å møte nye behov, forbedre eksisterende koder eller implementere nye funksjoner.

## Slik gjør du det:
```Elm
module Main exposing (..)
-- Importer alle nødvendige moduler og funksjoner
-- Ved å bruke "exposing (..)" inkluderer du alle funksjoner
-- som er tilgjengelige i den importerte modulen
import Html exposing (..)
-- En enkel "Hello World" funksjon
-- som skriver ut en hilsen til brukeren
-- koden nedenfor vil bli utført ved å kjøre 
-- `elm reactor` eller `elm make` kommandoen
main : Program () () ()
main =
    Html.text "Hei verden!"
```

## Dypdykk:
Historisk sett, var det å starte et nytt prosjekt en lang og kompleks prosess som krevde mye planlegging og ressurser. Med dagens programmeringsspråk som Elm, kan det nå gjøres raskt og effektivt. Det finnes også andre programmeringsspråk og verktøy som kan brukes til å starte et nytt prosjekt, men Elm tilbyr et elegant og funksjonelt alternativ.

## Se også:
- Elm dokumentasjon: https://guide.elm-lang.org/
- Elm forum: https://discourse.elm-lang.org/
- Elm pakker: https://package.elm-lang.org/
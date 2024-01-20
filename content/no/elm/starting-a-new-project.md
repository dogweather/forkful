---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt handler om å sette opp en grunnleggende struktur for koden din, et skall for å bygge din applikasjon på. Programmerere gjør dette for å organisere sine ideer og lette vedlikehold av koden.

## Hvordan:

Her er noen grunnleggende kodesnutter og eksempler for Elm:

```Elm
import Html exposing (text)

main =
  text "Hei, Verden!"

```

Når du kjører dette, vil du se "Hei, Verden!" utskrevet i nettleseren din.

For å starte et nytt Elm-prosjekt, trenger du elm compiler. Du kan installere det ved å kjøre kommandoen 'npm install -g elm' i terminalen din. Deretter, bare opprett en ny mappe og initialiser et nytt Elm-prosjekt med 'elm init'-kommando.


## Dypdykk

Elm, som ble lansert i 2012 av Evan Czaplicki, er en funksjonell programmeringsspråk for frontend utvikling. Det gir utviklere en robust, sikker og vedlikeholdbar kodebase. Alternativene til Elm inkluderer JavaScript og dens forskjellige biblioteker som React, Vue, og Angular. Men, Elm tilbyr fordeler som null runtime errors og innebygd pakkesystem. I Elm, hver eneste fil er en egen modul som kan importeres inn i en annen fil, noe som muliggjør en strukturert og modulerbar tilnærming til prosjektene dine.

## Se Også

1. Elm Offisielt Dokumentasjon: https://guide.elm-lang.org/
2. Elm GitHub Repository: https://github.com/elm/
3. ELM: En introduksjon - https://elm-lang.org/blog/blazing-fast-html-round-two
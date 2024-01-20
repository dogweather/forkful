---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Sammenslåing av strenger i Elm: En praktisk gjennomgang

## Hva og hvorfor?

Sammenslåing av strenger, eller _string concatenation_, handler om å sette sammen to eller flere strenger for å lage en ny kombinert streng. Dette gjøres vanligvis for å formatere og manipulere data som skal vises eller behandles videre.

## Hvordan:

Elm gir oss noen forskjellige måter å koble sammen strenger. La oss se på noen eksempler:

```Elm
-- Metode 1: Bruk av (++)
let tekst1 = "Hei, "
let tekst2 = "verden!"
let samletTekst = tekst1 ++ tekst2
```
Output: "Hei, verden!"

```Elm
-- Metode 2: Bruk av String.concat
let flerTekster = ["Hei, ", "vakre ", "verden!"]
let samletTekst = String.concat flerTekster
```
Output: "Hei, vakre verden!"

## Dypdykk 

Historisk sett, er `++` operatøren en ganske tradisjonell metode for å sammenføyning av strenger i mange programmeringsspråk, inkludert Elm. Den er enkel og lett å bruke, men hvis du jobber med lister av strenger, kan `String.concat` være en mer effektiv løsning. 

Alternative metoder inkluderer `String.join` som fungerer på samme måte som `String.concat`, men også lar deg sette inn en separator mellom hver streng. 

Detaljer rundt hvordan strenger sammenføyes i Elm avhenger av spesifikasjonene til Elm's runtime system. Generelt sett, både `++` og `String.concat` lager nye strenger siden strenger i Elm er uforanderlige, noe som betyr at de originale strengene beholder sine verdier.

## Se også:

Hvis du vil lære mer om strenger og deres funksjoner i Elm, sjekk ut disse ressursene:

1. Offisiell Elm dokumentasjon om strengfunksjoner: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng er prosessen der variabler, uttrykk eller funksjoner byttes ut med deres faktiske verdier inni en streng. Dette gjør vi for å dynamisk lage strenger, spesielt når vi trenger å inkludere variabeldata i dem.

## Hvordan gjøre det:
I Elm, bruker vi `++` operatoren for å bygge sammensatte strenger. Her er en kodeeksempel:
```Elm
firstname = "Ola"
lastname = "Nordmann"
greeting = "Hei, " ++ firstname ++ " " ++ lastname ++ ". Hvordan har du det?"
```
Når du kjører denne koden, vil `greeting` være "Hei, Ola Nordmann. Hvordan har du det?"

## Dyp Dykk:
Historisk sett ble strenginterpolering først brukt i programmeringsspråk på 1960-tallet, og har siden blitt en standardfunksjon i mange moderne språk. Selv om Elm ikke støtter direkte strenginterpolering, kan `++` operasjonen effektivt brukes for det samme formålet.

Alternativt til `++`, kan funksjonen `String.concat` brukes til å bygge sammensatte strenger fra en liste av strenger.
```Elm
name = String.concat ["Ola", " ", "Nordmann"]
```
Denne koden vil gi samme resultat som det tidligere eksemplet, "Ola Nordmann".

Fra implementeringsperspektivet, endrer ikke `++` operasjonen den opprinnelige strengen i Elm. I stedet lager den en ny streng ved å sette sammen de to operandene. Dette bidrar til den funksjonelle naturen til Elm ved å unngå "side-effekter".

## Se også:
1. ["++" Operator Documentasjon](https://package.elm-lang.org/packages/elm/core/latest/String#++)
2. [String.concat Funksjonsdokumentasjon](https://package.elm-lang.org/packages/elm/core/latest/String#concat)
3. [Elm Guide: Strenger](https://guide.elm-lang.org/core_language.html#strings)
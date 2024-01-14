---
title:    "Elm: Å bruke regulære uttrykk"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å bruke regulære uttrykk (også kjent som regex) i Elm-programmering. Regulære uttrykk er et kraftig verktøy for å søke, matche og manipulere tekststrenger. De kan være nyttige for å validere brukerinput, filtrere data, og generelt gjøre søk og erstatning i strenger mer effektive.

## Hvordan bruke regulære uttrykk i Elm

For å bruke regex i Elm, må du importere Regex-modulen og bruke funksjoner som `regex` og `match`. Her er et enkelt eksempel på hvordan du kan matche et uttrykk inne i en streng:

```elm
import Regex exposing (..)

regex = regex "hund"

match regex "Jeg elsker hunder" --> Ok "hund"
match regex "Dette er en katt" --> Err (Regex.Find Nothing)
```

Som du kan se vil `match` returnere en `Ok` verdi hvis det er en match, og en `Err` verdi hvis det ikke er noen match. Du kan også bruke en `Regex.replace` funksjon for å erstatte matched strenger med en annen verdi.

## Dypdykk i regulære uttrykk

Regulære uttrykk kan være komplekse og ha mange ulike syntaksregler. Det finnes ulike metoder for å bygge regex uttrykk, og det er viktig å forstå hvordan forskjellige symboler og metakarakterer brukes. Du kan lære mer om regulære uttrykk ved å lese Elm's offisielle dokumentasjon eller ved å utforske ulike eksempler og Regex generatorer online.

## Se også

* [Elm Regex dokumentasjon](https://package.elm-lang.org/packages/elm/regex/latest)
* [Regex101 - Online regex tester og generator](https://regex101.com/)
* [RegExr - Interaktiv regex editor og tester](https://regexr.com/)
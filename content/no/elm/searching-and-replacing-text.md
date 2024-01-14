---
title:                "Elm: Å søke og erstatte tekst"
simple_title:         "Å søke og erstatte tekst"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er en vanlig oppgave for programmerere å måtte søke og erstatte tekst i en kodebase. Dette kan være for å endre variabelnavn, oppdatere gamle syntaks eller rette skrivefeil. Det kan være tidkrevende og kjedelig, men heldigvis finnes det verktøy som kan hjelpe oss med dette.

## Slik gjør du det

Det finnes ulike måter å søke og erstatte tekst i Elm-programmeringsspråket. Her er et eksempel på hvordan du kan bruke `String.replace` funksjonen for å endre alle forekomster av "hei" til "hallo" i en streng:

```Elm
import String exposing (replace)

tekst = "Hei alle sammen!"
nyTekst = replace "hei" "hallo" tekst

-- output: "Hallo alle sammen!"
```

Hvis du trenger å gjøre dette for mer komplekse uttrykk, kan du bruke `Regex.replace` funksjonen som lar deg bruke regulære uttrykk for å finne og erstatte tekst. Her er et eksempel på hvordan du kan bruke den til å bare beholde tall i en streng:

```Elm
import Regex exposing (replace)

tekst = "123abc"
nyTekst = replace Regex.allNumbers " " tekst

-- output: "123"
```

## Dypdykk

Det er viktig å være klar over at disse funksjonene vil bare endre den første forekomsten av teksten, med mindre du bruker den optionale parameteren `all` for å endre alle forekomster. Når det kommer til regular expression, er det også viktig å merke seg at du må bruke Regex-biblioteket og importere det riktig for å kunne bruke `Regex.replace` funksjonen.

## Se også

- [Offisiell Elm dokumentasjon om String modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Offisiell Elm dokumentasjon om Regex modulen](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Elm Syntax 3.0 - Søk og Erstatt med RegEx (video på engelsk)](https://www.youtube.com/watch?v=XcjXCeOVJ1Y)
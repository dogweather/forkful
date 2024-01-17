---
title:                "Uttrekk av delstrenger"
html_title:           "Elm: Uttrekk av delstrenger"
simple_title:         "Uttrekk av delstrenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Hva er egentlig "ekstrahering av substrings"? Dette er når vi ønsker å hente ut en del av en tekststreng. Dette gjøres for å få tak i spesifikke deler av teksten, for eksempel for å analysere eller manipulere den. Det er en vanlig oppgave blant programvareutviklere, og kan være svært nyttig i mange situasjoner.

## Hvordan:

I Elm kan vi bruke funksjonen `String.slice start end` for å hente ut en del av en streng. Her er `start` en integer som representerer starten av substringen, og `end` er en integer som representerer slutten av substringen. Dette kan også kombineres med funksjonen `String.length` for å hente ut en substring fra slutten av en streng, ved å bruke en negativ verdi for `start` eller `end`.

Eksempel:
```Elm
import String exposing (slice, length)

-- Henter ut de tre første tegnene fra en streng
substring = slice 0 2 "Hei!"
-- Resultatet blir "He"

-- Henter ut de tre siste tegnene fra en streng
substring = slice (-3) (-1) "Hello!"
-- Resultatet blir "lo"
```

## Dykk ned:

Teksten "Substring" kan spores tilbake til programmeringsspråket "Snobol" fra 1960-tallet, som introduserte begrepet "SUBSTR". I tillegg til funksjonen `slice` i Elm er det også mulig å bruke den innebygde funksjonen `String.sub` for å hente ut enkelte tegn fra en streng. Det finnes også lignende funksjoner i andre programmeringsspråk som JavaScript og Java.

## Se også:

- [Offisiell dokumentasjon for `String` modulen i Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Definisjon av "Substring" på wikipedia.org](https://en.wikipedia.org/wiki/Substring)
- [Eksempel på bruk av `slice` og `length` i en utforskning av Fibonacci-sekvensen](https://programmers27.wordpress.com/2016/01/01/fibonacci-number/)
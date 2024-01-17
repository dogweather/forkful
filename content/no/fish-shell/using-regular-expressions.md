---
title:                "Å bruke regulære uttrykk"
html_title:           "Fish Shell: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Bruk av regulære uttrykk (regular expressions) er en essensiell del av programmering og dataanalyse. Det er en måte å søke og manipulere tekstbaserte data på ved å bruke spesielle uttrykk og mønstre. Programmere bruker regulære uttrykk for å gjøre søk og manipulasjoner mer effektive og presise.

## Hvordan:

En enkel måte å bruke regulære uttrykk i Fish Shell er ved å bruke kommandoen `grep`. Dette eksempelet vil søke etter alle ord som begynner på `f` og slutter på `ish` i en tekstfil:

```
fish_grep -eo "\bf\([[:alpha:]]*is\)h\b" sample.txt
```

Output fra denne kommandoen vil være en liste med alle matchende ord.

## Dykk dypere:

Regulære uttrykk har blitt brukt lenge i programmering og ble først introdusert av matematikeren Stephen Cole Kleene i 1956. Det finnes også andre verktøy som brukes til å søke og manipulere tekst, som for eksempel awk og sed. Fish Shell har innebygd støtte for regulære uttrykk gjennom kommandoen `grep` og har også mulighet for å bruke uttrykk i variabler og funksjoner.

## Se også:

For mer informasjon om regulære uttrykk og hvordan de kan brukes i Fish Shell, kan du sjekke ut disse lenkene:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [RegExr - et online verktøy for å teste regulære uttrykk](https://regexr.com/)
---
title:                "Clojure: Slette tegn som samsvarer med et mønster"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan vi finne oss selv i situasjoner der vi trenger å slette tegn som matcher et bestemt mønster i en streng. Dette kan være nyttig for å rydde opp i data, formatere tekst eller filtrere ut uønskede tegn. Å forstå hvordan man kan utføre denne handlingen i Clojure kan være nyttig for å effektivisere og optimalisere kode.

## Hvordan Du Gjør Det

I Clojure kan vi bruke funksjonen `clojure.string/replace` for å erstatte deler av en streng basert på et gitt mønster. For å slette tegn som matcher et mønster, kan vi bruke et tomt streng som erstatning. Her er et eksempel på hvordan dette vil se ut:

```Clojure
(require '[clojure.string :as str])
(str/replace "Hello world!" #"[aeiou]" "")
```
Dette vil gi oss outputen "Hll wrld!", siden alle vokaler i strengen har blitt slettet.

Vi kan også bruke et regex-uttrykk for å spesifisere hvilke tegn vi ønsker å slette. I dette eksempelet vil vi slette alle tall fra en streng:

```Clojure
(str/replace "I have 5 apples" #"\d" "")
```
Outputen vil da bli "I have apples".

## Dypdykk

I tillegg til å bruke `clojure.string/replace`, kan vi også bruke funksjonen `clojure.string/replace-first` for å slette kun det første tegnet som matcher mønsteret. Dette kan være nyttig hvis vi ønsker å fjerne kun en del av strengen.

Vi kan også kombinere flere mønstre ved å bruke `clojure.string/replace-pattern`. Dette vil tillate oss å slette ulike typer tegn fra en streng basert på flere forskjellige mønstre.

## Se Også

- [Clojure's string manipulation functions](https://clojuredocs.org/clojure.string)
- [Regular expressions in Clojure](https://clojuredocs.org/clojure.core/re-find)
- [Tutorial on replacing characters in strings in Clojure](https://www.guru99.com/clojure-string-manipulations.html)
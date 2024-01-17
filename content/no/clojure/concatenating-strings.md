---
title:                "Sammenføyning av strenger"
html_title:           "Clojure: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sammenkobling av strenger, også kjent som å legge sammen strenger eller konkatenering av strenger, er når to eller flere separate strenger blir kombinert til én streng. Dette er nyttig for å lage dynamiske tekster eller meldinger i programmering, som kan endre seg basert på forskjellige variabler eller betingelser.

## Slik gjør du det:
```Clojure
(def string1 "Hei")
(def string2 "verden!")
(str string1 ", " string2)
```
Output: "Hei, verden!"

## Dypdykk:
Historisk sett har konkatenering vært en vanlig teknikk for å lage dynamiske strenger. Det finnes også alternative metoder for å kombinere strenger, som f.eks. maltekst eller substitusjon. I Clojure er det flere innebygde funksjoner for å konkatenerere strenger, som `str`, `str-join` og `format`. Det kan også være nyttig å vite at strenger er immutable, dvs. uendrelige, i Clojure. Dette betyr at når en streng er opprettet, kan den ikke endres.

## Se også:
Offisiell Clojure-dokumentasjon for konkatenering av strenger:
https://clojuredocs.org/clojure.core/str
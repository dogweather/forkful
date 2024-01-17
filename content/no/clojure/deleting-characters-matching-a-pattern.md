---
title:                "Slette tegn som matcher et mønster"
html_title:           "Clojure: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er en vanlig praksis blant programmerere. Dette innebærer å fjerne spesifikke tegn eller tegnrekker fra en tekststreng basert på et gitt mønster. Dette er nyttig for å rydde opp i uønskede data eller formatere tekst på en bestemt måte.

## Hvordan:
For å slette tegn som matcher et mønster i Clojure, kan du bruke funksjonen ```clojure.string/replace``` og spesifisere både mønsteret og teksten du ønsker å redigere. La oss si at vi ønsker å fjerne alle tall fra en tekststreng, vi kan bruke følgende kode:

```Clojure
(clojure.string/replace "Hei123! Dette er en tekst123" #"\d" "")
```
Dette vil returnere strengen "Hei! Dette er en tekst".

## Dykke dypere:
Denne teknikken med å slette tegn som matcher et mønster har vært en del av programmering i lang tid. Mønstermatching ble først introdusert i Lisp-programmering på 1950-tallet og har siden blitt en viktig del av språk som Clojure. Alternativer til dette kan være å bruke regulære uttrykk eller å bruke innebygde funksjoner for å fjerne spesifikke tegn.

Når det gjelder implementeringen i Clojure, bruker ```clojure.string/replace``` funksjonen det populære Java-biblioteket Regular Expressions (regex) under panseret, som gir stor fleksibilitet og kraft til mønstermatching.

## Se også:
- Offisiell Clojure dokumentasjon for ```clojure.string/replace```: https://clojure.org/reference/strings#_replace
- En guide for å lære mer om mønstermatching i Clojure: https://clojure.org/guides/learn/functions_strings#_pattern_matching_functions
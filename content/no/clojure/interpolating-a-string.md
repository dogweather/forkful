---
title:                "Interpolering av en streng"
html_title:           "Clojure: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Interpolering av en streng i Clojure betyr å inkludere variabler, funksjoner eller uttrykk i en streng, slik at de blir evaluert og satt inn i strengen. Dette er en vanlig praksis blant programmere for å gjøre strenger mer dynamiske og tilpasse dem til forskjellige situasjoner.

## Hvordan:
```Clojure
(def name "Marie")
(def age 25)
(print "Hei, mitt navn er ${name} og jeg er ${age} år gammel.")
```

Output:
```Hei, mitt navn er Marie og jeg er 25 år gammel.```

## Dykk dypere:
Interpolering av strenger ble først introdusert i programmeringsspråket Perl i 1987, og har siden blitt adoptert av mange andre språk, inkludert Clojure. Alternativ til interpolering inkluderer konkatenering eller formatering av strenger med spesifikke metoder. Denne teknikken implementeres i Clojure med hjelp av funksjoner som `str` og `format`.  

## Se også:
- https://clojure.org/
- https://clojuredocs.org/
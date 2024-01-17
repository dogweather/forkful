---
title:                "Generering av tilfeldige tall"
html_title:           "Clojure: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

Randomnummergenerering med Clojure

## Hva & Hvorfor?
Generering av tilfeldige tall er en viktig del av programmering fordi det lar oss skape variabel data som kan brukes i ulike scenarioer. Dette er spesielt nyttig i spill, simuleringer og sikkerhetsapplikasjoner.

## Slik:
Bruk funksjonen ```(rand)``` for å generere et tilfeldig flyttall mellom 0 og 1. Dette kan deretter skaleres ved å multiplisere med ønsket område. For eksempel: ```(rand)``` * 100 vil gi et tilfeldig tall mellom 0 og 100. Du kan også bruke funksjonen ```(rand-int)``` for å generere et tilfeldig heltall.

## Dypdykk:
Generering av tilfeldige tall er en viktig del av datalogiens historie og har blitt brukt i ulike sammenhenger siden begynnelsen av datamaskinenes tidsalder. Det finnes også ulike alternativer til å bruke ```rand``` og ```rand-int``` i Clojure, som for eksempel [java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html). Videre er det viktig å være klar over at disse funksjonene ikke genererer ekte tilfeldige tall, men heller pseudo-tilfeldige tall basert på et startpunkt kalt "seed". Seedet kan endres ved å bruke funksjonen ```(set! clojure.core/*rand-seed* <numbers>)``` for å få ulike sekvenser av tilfeldige tall.

## Se også:
- [Offisiell Clojure dokumentasjon for ```rand``` og ```rand-int```](https://clojure.org/api/cheatsheet)
- [Artikkel om bruk av tilfeldige tall i datamaskiner](https://www.experiment-resources.com/random-number-generators-in-computers.html)
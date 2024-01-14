---
title:                "Clojure: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions (regex) er et uttrykksmønster som brukes i programmering for å søke og manipulere tekst. Det er nyttig å lære og bruke regex for å effektivisere programmeringsoppgaver som involverer tekstbehandling og dataanalyse. 

## Hvordan

```Clojure
;; Søke etter et spesifikt ord i en tekststreng
(re-find #"verden" "Hei verden!") 
;; Output:
"verden"

;; Endre tekst ved hjelp av regex
(clojure.string/replace "Hello world!" #"world" "everyone")
;; Output:
"Hello everyone!"

;; Hente ut tall fra en tekststreng
(re-matches #"\d+" "Jeg skal på konsert i morgen kl 8:00")
;; Output:
(["8"])

;; Bruke regex for å filtrere data
(filter #(re-matches #"\d{3}-\d{2}-\d{4}" (get-in % [:navn])) data-liste)
;; Output:
[{:navn "Trude Olsen", :fødselsnummer "123-45-6789"}
 {:navn "Per Hansen", :fødselsnummer "456-78-9123"}
 {:navn "Lise Berg", :fødselsnummer "789-01-2345"}]
```

## Deep Dive

Regex kan virke forvirrende og komplekst til å begynne med, men med litt øvelse vil man oppdage hvor nyttig det kan være. Her er noen tips for å bruke regex i Clojure:

- Husk at regex er en streng, så alltid bruke " " rundt uttrykket.
- #regexformerkyler bruker Clojure`s interne regex parser og er raskere enn java.util regex.
- #{ } kan brukes for å søke etter en gruppe av valgfrie karakterer.
- Regex kan også brukes med variabler og funksjoner, så det er mulig å dynamisk endre hva man leter etter.

## Se også

- [Offisiell dokumentasjon for regex i Clojure](https://clojure.org/api/cheatsheet#Regular%20Expressions)
- [Regex tutorial på norsk](https://www.ntnu.no/wiki/pages/viewpage.action?pageId=97555282)
- [Regex101 - nettsted for å teste og eksperimentere med regex](https://regex101.com/)
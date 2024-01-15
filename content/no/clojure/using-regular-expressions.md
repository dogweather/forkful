---
title:                "Å bruke regulære uttrykk"
html_title:           "Clojure: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har prøvd å finne en bestemt streng av tekst i en større tekstblokk, vet du hvor vanskelig og tidkrevende det kan være. Det er her regulære uttrykk kommer inn. Ved å bruke regulære uttrykk kan du enkelt og effektivt finne, erstatte og manipulere tekst uten å måtte gjøre manuelle søk og erstatninger.

## Hvordan

```Clojure
# Vi importerer regex biblioteket
(require '[clojure.string :as str])
# Vi bruker regex for å finne og erstatte en streng i en tekstblokk
(str/replace "Hei, mitt navn er John." #"John" "Peter")
```
Output: "Hei, mitt navn er Peter."

```Clojure
# Vi kan bruke regulære uttrykk for å finne alle tall i en streng
(re-seq #"\d+" "Jeg er 27 år gammel.")
```
Output: ("27")

```Clojure
# Regulære uttrykk kan også brukes til å validere input fra brukeren
(defn validate-age [age]
  (re-matches #"\d+" age)))
# Hvis brukeren skriver inn en ugyldig alder vil funksjonen returnere false
(validate-age "30") 
```
Output: true

## Dypdykk

Regulære uttrykk er basert på en syntaks som lar deg definere et mønster som en tekststreng må matche. Dette inkluderer spesialelementer som .* som representerer en hvilken som helst tekst, \d som representerer et tall og \w som representerer en bokstav eller et tall.

Det er viktig å merke seg at regulære uttrykk kan være krevende å lære, spesielt for nybegynnere i Clojure. Men de kan være svært kraftige verktøy når de brukes riktig. Det er også mange online ressurser tilgjengelig for å hjelpe deg med å lære mer om regulære uttrykk og hvordan du kan implementere dem i dine Clojure-prosjekter.

## Se også

- [Offisiell Clojure dokumentasjon](https://clojure.org/api/cheatsheet)
- [RegExr - online verktøy for å teste og eksperimentere med regulære uttrykk](https://regexr.com/)
- [Regulære uttrykk i Clojure](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)
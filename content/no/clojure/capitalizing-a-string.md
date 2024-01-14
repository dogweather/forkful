---
title:                "Clojure: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å gjøre tekst til store bokstaver i Clojure? Vel, det kan forbedre lesbarheten av koden din og hjelpe deg med å representere ord eller setninger på en mer konsistent måte. Det kan også være en del av en strengmanipulasjonsfunksjon som du trenger i prosjektet ditt.

## Hvordan

For å gjøre en tekst til store bokstaver i Clojure, kan du bruke funksjonen ```clojure (clojure.pprint/capitalize) ```. Her er et eksempel på bruk av denne funksjonen:

```clojure
(clojure.pprint/capitalize "dette er en test")
```

Dette vil produsere følgende output:

```clojure
"Dette er en test"
```
En annen måte å gjøre en tekst til store bokstaver på, er å bruke ```clojure (clojure.string/upper-case)``` funksjonen. Denne funksjonen gjør hele strengen til store bokstaver, uavhengig av hvor mange ord den inneholder.

## Dypdykk

Begge metodene som ble nevnt ovenfor, bruker standard språksensitivitet for å konvertere tekst til store bokstaver. Dette betyr at de vil ta hensyn til grammatikk og språkregler når de utfører konverteringen.

Men, hvis du ønsker å gjøre om en streng til store bokstaver uten å ta hensyn til språksensitivitet, kan du bruke ```clojure (clojure.string/upper-case* )``` funksjonen. Denne funksjonen vil konvertere alle tegn i strengen til store bokstaver, uavhengig av språkkontekst.

## Se også

- [Documentation for ```clojure.pprint/capitalize```](https://clojuredocs.org/clojure.pprint/capitalize)
- [Documentation for ```clojure.string/upper-case```](https://clojuredocs.org/clojure.string/upper-case)
- [Documentation for ```clojure.string/upper-case*```](https://clojuredocs.org/clojure.string/upper-case*)
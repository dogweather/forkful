---
title:                "Clojure: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å arbeide med JSON i Clojure er enkelt og effektivt. JSON er en populær format for å utveksle data mellom systemer, og ved å lære å jobbe med det i Clojure, kan du utvikle mer robust og fleksibel programvare.

## Hvordan

For å jobbe med JSON i Clojure, trenger du først og fremst å importere biblioteket "clojure.data.json". Deretter kan du bruke funksjonen "json/read" for å lese JSON-data fra en fil eller en streng:

```Clojure
(require '[clojure.data.json :as json])

(json/read (clojure.java.io/reader "data.json"))
```

Dette vil returnere Clojure-datastrukturen som representerer JSON-strukturen. Du kan også bruke funksjonen "json/write" for å skrive data til en JSON-fil:

```Clojure
(require '[clojure.data.json :as json])

(json/write {:name "Tom", :age 25} (clojure.java.io/writer "data.json"))
```

Dette vil skrive dataene til en JSON-fil med navnet "data.json". Se på Clojure sin offisielle dokumentasjon for mer informasjon om ulike funksjoner og muligheter for å arbeide med JSON.

## Dypdykk

Å jobbe med JSON i Clojure er veldig fleksibelt, da strukturen av JSON og Clojure-data er veldig lik. Dette gjør det enkelt å manipulere og behandle data. I tillegg til å lese og skrive JSON-data, kan du også bruke Clojure sine innebygde funksjoner, som "assoc" og "get", for å endre og hente dataene fra JSON-strukturen.

En annen nyttig funksjon er "json/str", som lar deg konvertere Clojure-datastrukturen til en streng som representerer JSON-data. Dette kan være nyttig når du kommuniserer med eksterne API-er eller systemer som bare støtter JSON-format.

## Se også

- Offisiell dokumentasjon for å arbeide med JSON i Clojure: https://github.com/clojure/data.json
- Sammenligning av forskjellige biblioteker for å arbeide med JSON i Clojure: https://github.com/funcool/clojure-json-benchmark
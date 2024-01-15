---
title:                "Å jobbe med json"
html_title:           "Clojure: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data i Clojure, spesielt med å hente, lagre og manipulere data fra eksterne APIer, vil du mest sannsynlig komme over JSON-formatet. JSON (JavaScript Object Notation) er et populært format for å utveksle data mellom forskjellige programmeringsspråk og plattformer. I Clojure er det flere måter å jobbe med JSON på, og det kan være nyttig å vite hvordan du kan håndtere dette formatet for å gjøre arbeidsflyten din mer effektiv.

## Hvordan

For å arbeide med JSON i Clojure, trenger du å bruke et bibliotek som heter "clj-json". Dette biblioteket gjør det enkelt å håndtere JSON-data i Clojure, ved å konvertere mellom JSON-objekter og Clojure-datastrukturer. La oss se på noen eksempler på hvordan du kan bruke dette biblioteket for å hente og manipulere JSON-data.

Først må du importere biblioteket i Clojure-prosjektet ditt ved å legge til følgende linje i prosjektets "deps.edn" fil:

```Clojure
{:deps {clj-json {:mvn/version "1.0.2"}}}
```

Her er et eksempel på hvordan du kan hente JSON-data fra et API og konvertere det til en Clojure-map:

```Clojure
(require '[clj-json.core :as json])

(def api-response (slurp "https://example.com/api/data")) ;Henter JSON-data fra API
(def data (json/parse-string api-response)) ;Konverterer data til Clojure-map
```

Du kan da gå gjennom dataene som en vanlig Clojure-map og hente ut nødvendig informasjon. Her er et eksempel på hvordan du kan hente ut verdiene fra et JSON-objekt:

```Clojure
(get data "key") ;Henter ut verdien til "key"
```

Du kan også bruke biblioteket til å konvertere Clojure-datastructurer til JSON-format. Her er et eksempel på hvordan du kan lage et JSON-objekt fra en Clojure-map og skrive det til en fil:

```Clojure
(def map-data {:name "John", :age 30})
(def json-data (json/write-str map-data)) ;Konverterer map til JSON-string
(spit "output.json" json-data) ;Skriver til fil
```

## Dykk dypere

Clj-json biblioteket har også flere funksjoner for å håndtere mer kompleks JSON-data, som nestede objekter og arrays. Du kan lese mer om disse funksjonene i dokumentasjonen til biblioteket. Det finnes også alternativer til clj-json biblioteket, som "cheshire" og "data.json", men de fleste av dem tilbyr lignende funksjonalitet for å arbeide med JSON i Clojure.

## Se også

- [Offisiell dokumentasjon for clj-json biblioteket](https://github.com/funcool/clojure-json)
- [Clojure for the Brave and True - Chapter 14 (Introducing JSON)](https://www.braveclojure.com/writing-a-clojure-application/introducing-json/)
- [Dokumentasjon for cheshire biblioteket](https://github.com/dakrone/cheshire)
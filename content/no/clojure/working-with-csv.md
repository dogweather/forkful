---
title:                "Å jobbe med csv"
html_title:           "Clojure: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
CSV (Comma Separated Values) er en vanlig filformat for å lagre tabellignende data. Det er ofte brukt i dataanalyse, regneark og databasesystemer. Ved å kunne jobbe med CSV-filer, kan du enkelt importere og eksportere data mellom ulike programmeringsverktøy og systemer.

## Slik gjør du
For å jobbe med CSV-filer i Clojure, kan du bruke biblioteket "clojure-csv". Her er et eksempel på hvordan du kan åpne en CSV-fil og lese dataene:

```Clojure
(require '[clojure-csv.core :as csv])

(with-open [file (clojure.java.io/reader "data.csv")]
  (doall (csv/read-csv file)))
```

I dette eksempelet bruker vi funksjonen `read-csv` til å lese dataene fra filen "data.csv". Utdataen vil være en liste av lister, der hver liste representerer en rad i CSV-filen. Du kan også bruke parametere for å spesifisere separator og header-rader, se dokumentasjonen for mer informasjon.

Når du har lest dataene inn i Clojure, kan du enkelt manipulere dem ved hjelp av Clojures funksjoner og datastrukturer. Du kan for eksempel filtrere ut rader basert på bestemte kriterier, endre verdier eller lage en ny CSV-fil med resultatet.

## Dypdykk
I tillegg til å lese og skrive CSV-filer, kan du også bruke "clojure-csv" til å validere og formatere dataene. Det finnes også andre biblioteker som kan være nyttige for å håndtere CSV-filer, som for eksempel "data.csv" og "data.zip".

En annen ting å være oppmerksom på når du jobber med CSV-filer, er at data kan være formatert på ulike måter. Noen ganger kan det være nødvendig å utføre en del rensing og formatering på dataene før du kan jobbe med dem i Clojure.

## Se også
- [ClojureScript - en introduksjon](https://www.datametrix.no/blog/clojurescript-en-introduksjon/) 
- [Hvordan hente og behandle data i Clojure](https://www.datametrix.no/blog/hvordan-hente-og-behandle-data-i-clojure/)
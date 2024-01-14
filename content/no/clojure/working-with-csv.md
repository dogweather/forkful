---
title:                "Clojure: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV-filer er en vanlig oppgave for mange som driver med programmering, spesielt for de som jobber med dataanalyse og databehandling. CSV står for "Comma Separated Values" og er en type filformat som brukes for å lagre og utveksle strukturerte data. Clojure gir et kraftig og enkelt verktøy for å jobbe med slike filer, og i denne bloggposten skal vi se på hvordan man kan gjøre det.

## Hvordan

Å arbeide med CSV-filer i Clojure er enkelt og intuitivt. Først må du importere "clojure.data.csv"-biblioteket ved å legge til følgende kode øverst i filen:

```Clojure
(require '[clojure.data.csv :as csv])
```

Deretter kan du lese inn en CSV-fil ved å bruke funksjonen `csv/read-csv`. Denne funksjonen tar inn en streng som representerer filbanen til den ønskede filen. For eksempel:

```Clojure
(def csv-data (csv/read-csv "mittdokument.csv"))
```

Du kan også angi en bekvemmelighetsfunksjon for å behandle CSV-data på en enklere måte. For eksempel:

```Clojure
(defn les-csv [filnavn]
  (with-open [fil (clojure.java.io/reader filnavn)]
    (doall (csv/read-csv fil))))
```

Vi har nå laget en funksjon som tar inn et filnavn, åpner filen og leser inn CSV-dataene på en trygg måte. For å finne ut hvilke kolonner som finnes i CSV-filen, kan vi bruke funksjonen `csv/csv-keys`:

```Clojure
(csv/csv-keys csv-data)
```

Dette vil gi oss en liste over kolonnenavn, som vi kan bruke til å hente ut spesifikke kolonner fra dataene. For eksempel, for å hente ut all data fra kolonnen "navn", kan vi gjøre følgende:

```Clojure
(map :navn csv-data)
```

## Dypdykk

Clojure tilbyr flere nyttige funksjoner for å jobbe med CSV-data. For eksempel, hvis du ønsker å filtrere ut rader som oppfyller et visst kriterium, kan du bruke funksjonen `filter`. Dette kan være veldig nyttig når du jobber med store og komplekse CSV-filer.

En annen nyttig funksjon er `csv/write-csv`, som lar deg lagre dataene fra en CSV-fil til en ny fil. Ved hjelp av denne funksjonen kan du også enkelt manipulere og formatere dataene før du lagrer dem.

## Se også

* [Offisiell dokumentasjon for clojure.data.csv](https://clojure.github.io/data.csv/)
* [ClojureDocs - csv](https://clojuredocs.org/clojure.data.csv)
* [En introduksjon til Clojure](https://www.duo.uio.no/bitstream/handle/10852/64480/446_artikkel_Luksch.pdf?sequence=1&isAllowed=y) (på norsk)
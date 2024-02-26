---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:04.650386-07:00
description: "\xC5 jobbe med CSV-filer (Comma-Separated Values) inneb\xE6rer parsing\
  \ og generering av tekstdata strukturert som rader og kolonner, likt som regnearkdata.\u2026"
lastmod: '2024-02-25T18:49:38.651732-07:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer (Comma-Separated Values) inneb\xE6rer parsing og\
  \ generering av tekstdata strukturert som rader og kolonner, likt som regnearkdata.\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med CSV-filer (Comma-Separated Values) innebærer parsing og generering av tekstdata strukturert som rader og kolonner, likt som regnearkdata. Denne prosessen er essensiell for datautveksling mellom applikasjoner, databaser, og for oppgaver knyttet til datatransformasjon, på grunn av CSVs brede adopsjon som et lettvekt, interoperabelt format.

## Hvordan:

### Lese en CSV-fil
Clojure har ikke innebygd CSV-parsing i sitt standardbibliotek, men du kan bruke `clojure.data.csv` biblioteket til dette formålet. Først, legg til biblioteket i prosjektavhengighetene dine.

I din `project.clj`, legg til denne avhengigheten:
```clojure
[clojure.data.csv "1.0.0"]
```
For å lese en CSV-fil og skrive ut hver rad:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "sti/til/dinfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Dette vil skrive ut hver rad av CSV-en som en Clojure-vektor.

### Skrive til en CSV-fil
For å skrive data til en CSV-fil, kan du bruke det samme `clojure.data.csv`-biblioteket:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "navn" "alder"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "sti/til/utdatafil.csv")]
    (csv/write-csv writer data)))
```
Dette oppretter eller overskriver `utdatafil.csv`, og fyller den med de angitte dataene.

### Bruk av et tredjepartsbibliotek: `clojure.data.csv`

Selv om `clojure.data.csv` er utvilsomt det mest rettframme biblioteket for håndtering av CSV i Clojure, for mer komplekse oppgaver, som å håndtere CSV-er med spesialtegn eller uvanlige skilletegn, kan du utforske flere alternativer innen økosystemet eller til og med vurdere Java interop med biblioteker som Apache Commons CSV. Imidlertid, for de fleste standard CSV-behandlingsoppgaver i Clojure, tilbyr `clojure.data.csv` et enkelt og effektivt verktøysett.

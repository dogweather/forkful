---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV står for komma-separerte verdier, en filformat brukt for å lagre tabell-data. Programmerere bruker det fordi det er universelt, lett å lese og skrive, og det kan brukes av mange forskjellige programmer.

## How to:
Å jobbe med CSV i Clojure er enkelt med `clojure.data.csv` biblioteket. Her er hvordan du leser og skriver CSV-data:

```Clojure
;; Legg til nødvendig bibliotek
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

;; Lese CSV-fil
(with-open [reader (io/reader "data.csv")]
  (let [data (csv/read-csv reader)]
    (println data)))  ; Skriver ut CSV-data som en liste av rader

;; Skrive til CSV-fil
(with-open [writer (io/writer "data.csv")]
  (csv/write-csv writer [["ID", "Navn", "Alder"] ["1", "Ola", "42"] ["2", "Kari", "36"]]))
```
Eksempeloutput:
```
(["ID" "Navn" "Alder"] ["1" "Ola" "42"] ["2" "Kari" "36"])
```

## Deep Dive
CSV-formatet har vært brukt siden 1970-tallet og er fortsatt populært på grunn av dets enkelhet. Alternativer inkluderer JSON og XML, men de er mer komplekse. I Clojure kan `clojure.data.csv` håndtere de fleste CSV-relaterte oppgaver. Det er viktig å håndtere felter som inneholder komma eller nye linjer riktig, ved å omslutte dem med anførselstegn.

## See Also
- Clojure.data.csv dokumentasjon: https://clojure.github.io/data.csv/
- Clojure for the Brave and True (bok): https://www.braveclojure.com/
- Clojure.org: https://clojure.org/

---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV är textfiler där data separeras med kommatecken. Programmerare hanterar CSV för att enkelt utbyta och manipulera stora datamängder.

## How to:
```Clojure
; Lägga till clojure.data.csv och java.io för filhantering
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

; Läsa CSV-fil och skriva ut varje rad
(with-open [reader (io/reader "data.csv")]
  (doseq [line (csv/read-csv reader)]
    (println line)))

; Skapa och skriva till en CSV-fil
(let [data [["namn", "ålder"], ["Alice", 30], ["Bob", 25]]
      writer (io/writer "output.csv")]
  (csv/write-csv writer data)
  (.flush writer)
  (.close writer))

; Exempeloutput för att läsa CSV
; ["namn" "ålder"]
; ["Alice" "30"]
; ["Bob" "25"]
```

## Deep Dive
CSV står för Comma-Separated Values och har använts sedan 1970-talet. Alternativ till CSV inkluderar JSON och XML som stöder komplex datastruktur. I Clojure kan du hantera CSV med `clojure.data.csv`-biblioteket som hanterar enkla till komplexa operationer, dock med en plattare struktur jämfört med JSON.

## See Also
- Clojure Documentation: https://clojure.org/
- `clojure.data.csv` library: https://github.com/clojure/data.csv
- More about CSV: https://tools.ietf.org/html/rfc4180

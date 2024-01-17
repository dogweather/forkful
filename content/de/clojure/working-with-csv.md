---
title:                "Arbeiten mit CSV"
html_title:           "Clojure: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit CSV (Comma-Separated Values) ist eine gängige Aufgabe für Programmierer. CSV ist ein Dateiformat, das verwendet wird, um tabellarische Daten zu speichern und zu bearbeiten. Programmierer nutzen CSV, um Daten in einem einfach zu lesenden und zu bearbeitenden Format zu organisieren und zu speichern.

## Wie geht's:
```Clojure
;; Eine CSV-Datei lesen
(require '[clojure.java.io :as io])
(require '[clojure-csv.core :as csv])

(with-open [reader (io/reader "beispiel.csv")]
  (doall (csv/read-csv reader)))

;; Eine CSV-Datei schreiben
(require '[clojure.string :as str])
(require '[clojure-csv.core :as csv])

(with-open [writer (io/writer "beispiel.csv")]
  (csv/write-csv writer [["Spalte 1" "Spalte 2" "Spalte 3"]
                         ["Wert 1" "Wert 2" "Wert 3"]
                         ["Wert 4" "Wert 5" "Wert 6"]]))
```
```
Spalte 1,Spalte 2,Spalte 3
Wert 1,Wert 2,Wert 3
Wert 4,Wert 5,Wert 6
```

## Tiefer eintauchen:
CSV wurde in den 1970er Jahren entwickelt, um den Austausch von Tabellendaten zwischen verschiedenen Programmen zu ermöglichen. Heute ist es ein weit verbreitetes und standardisiertes Dateiformat. Es gibt auch alternative Formate wie TSV (Tab Separated Values) oder JSON (JavaScript Object Notation). In Clojure wird die Bibliothek "clojure-csv" verwendet, um mit CSV-Dateien zu arbeiten.

## Siehe auch:
- Offizielle Dokumentation von clojure-csv: https://github.com/clojure/data.csv
- Artikel über CSV in der Programmiersprache Python: https://realpython.com/python-csv/
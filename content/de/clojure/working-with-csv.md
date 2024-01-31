---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
CSV (Comma-separated values) Dateien sind eine weit verbreitete Art, tabellarische Daten zu speichern. Programmierer nutzen CSV, weil es ein simples, textbasiertes Format ist, das von den meisten Datenverarbeitungsprogrammen gelesen und geschrieben werden kann.

## How to: (Wie geht das:)
```Clojure
;; CSV-Datei einlesen
(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])

(with-open [reader (io/reader "daten.csv")]
  (let [data (csv/read-csv reader)]
    (println data)))

;; Beispiel Ausgabe: [["Spalte1" "Spalte2" "Spalte3"] ["Wert1" "Wert2" "Wert3"]]

;; CSV-Datei schreiben
(with-open [writer (io/writer "ausgabe.csv")]
  (csv/write-csv writer [["Spalte1" "Spalte2" "Spalte3"] ["Wert1" "Wert2" "Wert3"]]))
```

## Deep Dive (Tiefer eintauchen)
CSV wurde ursprünglich in den frühen 1970ern entwickelt und ist bis heute populär wegen seiner Simplizität. Alternativen wie JSON oder XML bieten mehr Struktur und Funktionalität, können aber komplexer in der Verarbeitung sein. In Clojure erfolgt der Umgang mit CSV vorrangig über die `clojure.data.csv` Bibliothek, welche auf die Java-Bibliothek `opencsv` zurückgreift und Clojure-spezifische Funktionalität bietet.

## See Also (Siehe auch)
- Clojure Data CSV GitHub repository: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
- OpenCSV Bibliothek: [http://opencsv.sourceforge.net/](http://opencsv.sourceforge.net/)
- Weitere Informationen zum CSV-Format: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)

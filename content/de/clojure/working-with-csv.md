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

## Warum

CSV (Comma-Separated Values) Dateien sind ein wichtiges Format, um Daten zu speichern und auszutauschen. Als Programmierer ist es wichtig, die Grundlagen der Arbeit mit CSV Dateien zu verstehen, um effizient und genau mit Daten umgehen zu können.

## Wie

Um eine CSV Datei in Clojure einzulesen, verwenden wir die "clojure.java.io" Bibliothek und die Funktion "read-csv", um die Daten in eine Liste von Listen zu konvertieren. Hier ist ein Beispielcode, der eine CSV Datei mit Produkten einliest:

```Clojure
(ns csv-beispiel.core
  (:require [clojure.java.io :as io]))
  
(def produkte (with-open [produkte-datei (io/reader "produkte.csv")]
                 (doall (read-csv produkte-datei))))

(println produkte)
```

Die Ausgabe dieses Codes ist eine Liste von Listen, wobei jede Liste eine Zeile aus der CSV Datei repräsentiert. Zum Beispiel könnte die Ausgabe folgendermaßen aussehen:

```
([Name Preis Farbe] [T-Shirt 20 Blau] [Shorts 30 Schwarz] [Hemd 40 Weiß])
```

Um Daten in eine CSV Datei zu schreiben, können wir die "write-csv" Funktion verwenden. Hier ist ein Beispielcode, der die Daten aus der vorherigen Liste in eine neue CSV Datei schreibt:

```Clojure
(ns csv-beispiel.core
  (:require [clojure.java.io :as io]))
  
(def produkte (with-open [produkte-datei (io/writer "neue-produkte.csv")]
                 (doall (write-csv produkte-datei produkte))))
```

Dieser Code erstellt eine neue CSV Datei mit dem Namen "neue-produkte.csv", die die gleichen Daten wie die ursprüngliche Datei enthält.

## Deep Dive

Es gibt viele Möglichkeiten, um mit CSV Dateien in Clojure zu arbeiten. Die "clojure.data.csv" Bibliothek bietet beispielsweise Funktionen zum Konvertieren von CSV Daten in andere Formate wie Maps oder Vektoren. Es gibt auch Bibliotheken, wie z.B. "clojure-csv", die zusätzliche Funktionen zum Lesen und Schreiben von CSV Dateien bieten.

Ein wichtiger Punkt beim Arbeiten mit CSV Dateien ist auch die Behandlung von Sonderfällen wie leeren Zeilen oder Spalten, fehlenden Werten und unterschiedlichen Datentypen. Es ist wichtig, diese Fälle beim Lesen und Schreiben von CSV Dateien zu berücksichtigen, um Fehler im Programm zu vermeiden.

## Siehe auch

- [Clojure Dokumentation zu CSV](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/read-csv)
- [Clojure CSV Bibliothek](https://github.com/danmidwood/clojure-csv)
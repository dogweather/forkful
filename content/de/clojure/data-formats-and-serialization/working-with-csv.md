---
aliases:
- /de/clojure/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:04.668835-07:00
description: "Die Arbeit mit CSV-Dateien (Comma-Separated Values, Werte getrennt durch\
  \ Kommas) umfasst das Parsen und Erzeugen von Textdaten, die als Reihen und Spalten\u2026"
lastmod: 2024-02-18 23:09:04.530983
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV-Dateien (Comma-Separated Values, Werte getrennt durch\
  \ Kommas) umfasst das Parsen und Erzeugen von Textdaten, die als Reihen und Spalten\u2026"
title: Arbeiten mit CSV
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV-Dateien (Comma-Separated Values, Werte getrennt durch Kommas) umfasst das Parsen und Erzeugen von Textdaten, die als Reihen und Spalten strukturiert sind, ähnlich wie Tabellendaten. Dieser Prozess ist essenziell für den Datenaustausch zwischen Anwendungen, Datenbanken und für Datentransformationsaufgaben, aufgrund der weiten Verbreitung von CSV als einem leichten, interoperablen Format.

## Wie man es macht:

### Eine CSV-Datei lesen
Clojure hat keine eingebaute CSV-Parsing-Funktion in seiner Standardbibliothek, aber Sie können dafür die Bibliothek `clojure.data.csv` verwenden. Fügen Sie zuerst die Bibliothek zu Ihren Projekt-Abhängigkeiten hinzu.

In Ihrer `project.clj` fügen Sie die folgende Abhängigkeit hinzu:
```clojure
[clojure.data.csv "1.0.0"]
```
Um eine CSV-Datei zu lesen und jede Reihe auszudrucken:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "Pfad/zu/deinerDatei.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Dies gibt jede Reihe der CSV-Datei als Clojure-Vektor aus.

### In eine CSV-Datei schreiben
Um Daten in eine CSV-Datei zu schreiben, können Sie dieselbe `clojure.data.csv` Bibliothek verwenden:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id", "name", "age"]
            ["1", "John Doe", "28"]
            ["2", "Jane Doe", "31"]]]
  (with-open [writer (io/writer "Pfad/zu/Ausgabedatei.csv")]
    (csv/write-csv writer data)))
```
Dies erstellt oder überschreibt `Ausgabedatei.csv`, und füllt sie mit den angegebenen Daten.

### Verwendung einer Drittanbieterbibliothek: `clojure.data.csv`

Obwohl `clojure.data.csv` wohl die unkomplizierteste Bibliothek für den Umgang mit CSV in Clojure ist, könnten Sie für komplexere Aufgaben, wie den Umgang mit CSVs mit Sonderzeichen oder unkonventionellen Trennzeichen, zusätzliche Optionen im Ökosystem erkunden oder sogar Java-Interop mit Bibliotheken wie Apache Commons CSV in Betracht ziehen. Jedoch stellt `clojure.data.csv` für die meisten Standard CSV-Verarbeitungsaufgaben in Clojure ein einfaches und effektives Werkzeug-set bereit.

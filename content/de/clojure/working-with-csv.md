---
title:                "Clojure: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Datenverarbeitung ist das CSV-Format (Comma-Separated Values) ein häufig verwendetes Werkzeug. CSV-Dateien können Daten strukturiert und leicht lesbar speichern, was sie zu einer idealen Wahl für die Arbeit mit großen Datensätzen macht. In dieser Blog-Post werden wir uns genauer ansehen, warum es sich lohnt, sich mit dem Arbeiten mit CSV in Clojure zu beschäftigen.

## Wie geht man vor

Um mit CSV-Dateien in Clojure zu arbeiten, gibt es eine Reihe von nützlichen Bibliotheken wie z.B. `clojure.data.csv` und `clojure-csv`, die das Parsen von CSV-Dateien vereinfachen und es uns ermöglichen, die Daten in Clojure direkt zu verwenden. Schauen wir uns ein Beispiel an:

```Clojure
(require '[clojure.data.csv :as csv])

(with-open [reader (csv/open "daten.csv")]
  (doall (csv/read-csv reader)))
```

Die `with-open` Funktion stellt sicher, dass die Datei nach dem Lesen automatisch geschlossen wird. Der `doall` Ausdruck liest die CSV-Datei Zeile für Zeile und gibt eine Liste von Listen zurück, wobei jeder Eintrag eine Zeile der Datei darstellt. Hier ist ein Beispiel für die Ausgabe einer CSV-Datei:

```Clojure
[["Name" "Alter" "Stadt"]
["Maria" "34" "Berlin"]
["Hans" "27" "München"]
["Lena" "41" "Hamburg"]]
```

## Tiefere Einblicke

Eine wichtige Sache, die man beim Arbeiten mit CSV-Dateien beachten sollte, ist das Encoding der Datei. Standardmäßig werden CSV-Dateien in einem bestimmten Encoding gespeichert, daher ist es wichtig, beim Lesen der Datei das entsprechende Encoding anzugeben. Zum Beispiel:

```Clojure
(with-open [reader (clojure.java.io/reader "daten.csv" :encoding "UTF-8")]
  (doall (csv/read-csv reader)))
```

Eine weitere nützliche Funktion ist `write-csv`, mit der wir CSV-Dateien schreiben können. Hier ein Beispiel:

```Clojure
(csv/write-csv "neue-daten.csv" [[:name :age :city]
                                ["Peter" 29 "Düsseldorf"]
                                ["Sabine" 38 "Frankfurt"]])
```

In diesem Fall wird eine neue CSV-Datei namens "neue-daten.csv" erstellt, die folgenden Inhalt hat:

```Clojure
[["name" "age" "city"]
["Peter" 29 "Düsseldorf"]
["Sabine" 38 "Frankfurt"]]
```

## Siehe Auch

* Offizielle Clojure Dokumentation zu `clojure.data.csv`: http://clojure.github.io/clojure/clojure.data.csv-api.html
* Clojure-Werkzeugbibliothek `parsing`: https://github.com/clojure/tools.parsing
* Beispiel für CSV-Dateien zum Üben: https://people.sc.fsu.edu/~jburkardt/data/csv/csv.html
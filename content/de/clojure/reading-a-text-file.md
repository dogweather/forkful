---
title:                "Eine Textdatei lesen"
html_title:           "Clojure: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schonmal ein Computerprogramm geschrieben hast, weißt du, dass es oft notwendig ist, Daten aus einer externen Datei zu lesen. Egal ob es sich um Benutzereingaben, Konfigurationsdateien oder andere Informationen handelt, das Lesen von Textdateien ist eine wichtige Fähigkeit für jeden Programmierer.

## So geht's

In Clojure gibt es verschiedene Möglichkeiten, eine Textdatei zu lesen. Eine davon ist die Verwendung der Funktion `slurp`, die den Inhalt einer Datei als String zurückgibt. Hier ist ein Beispiel:

```Clojure
(def text (slurp "meine-datei.txt"))
```

In diesem Beispiel haben wir eine Textdatei mit dem Namen "meine-datei.txt" erstellt und den Inhalt in einer Variablen namens `text` gespeichert. Wir können nun auf den Inhalt der Datei zugreifen, indem wir die Variable verwenden.

Wenn du den Inhalt der Datei zeilenweise lesen möchtest, kannst du die Funktion `line-seq` verwenden, die eine Sequenz von Zeilen aus der Datei zurückgibt. Hier ist ein Beispiel:

```Clojure
(def lines (line-seq (java.io.BufferedReader. (clojure.java.io/reader "meine-datei.txt"))))
```

In diesem Beispiel verwenden wir die Java-Klasse `BufferedReader` zusammen mit `line-seq`, um den Inhalt der Textdatei Zeile für Zeile in einer Sequenz zu speichern.

## Tiefentauchgang

Beim Lesen von Textdateien ist es wichtig zu beachten, dass der Inhalt als String oder Sequenz von Strings zurückgegeben wird. Wenn du jedoch bestimmte Daten aus der Datei extrahieren möchtest, musst du diese Strings möglicherweise in andere Datenstrukturen wie Vektoren oder Maps umwandeln.

Eine Möglichkeit, dies zu tun, ist die Verwendung der Funktion `clojure.string` zusammen mit regulären Ausdrücken. Beispiel:

```Clojure
(require '[clojure.string :as str])

(def text "Name: Max Mustermann Alter: 25")

(str/split text #"Alter: ([0-9]+)")
```

In diesem Beispiel verwenden wir `str/split`, um den Inhalt der Textdatei an der Stelle zu trennen, an der das Wort "Alter:" und eine beliebige Anzahl an Zahlen folgen. Diese Funktion gibt ein Tupel mit dem entsprechenden Namen und Alter zurück.

Eine andere Möglichkeit besteht darin, die Dateizeilen zunächst in Maps zu konvertieren und dann die gewünschten Daten aus den Maps zu extrahieren. Hier ist ein Beispiel:

```Clojure
(def lines (line-seq (java.io.BufferedReader. (clojure.java.io/reader "meine-datei.txt"))))

(def data (map #(clojure.edn/read-string %) lines))

(get (first data) :name)
```

In diesem Beispiel verwenden wir `map` zusammen mit `clojure.edn/read-string`, um die Dateizeilen in Maps umzuwandeln. Dann können wir mit `get` den Wert des Schlüssels `:name` aus der ersten Map abrufen.

## Siehe auch

- [Clojure Dokumentation zu Dateioperationen](https://clojure.org/guides/io)
- [Clojure `clojure.string` Referenz](https://clojure.github.io/clojure/clojure.string-api.html)
- [RegExr - Tool für die Erstellung und Prüfung von regulären Ausdrücken](https://regexr.com/)
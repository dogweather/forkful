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

## Was & Warum?

Wenn wir von Lesen einer Textdatei sprechen, meinen wir das Einlesen bzw. Importieren von Inhalten aus einer solchen Datei. Programmierer nutzen dies, um zum Beispiel Daten in eine Datenbank zu importieren oder um Dateien in ein anderes Format zu konvertieren. 

## Wie geht's?

Die grundlegende Funktion, um eine Textdatei in Clojure zu lesen, ist ```read-string```. Sie nimmt als Argument den Dateipfad und gibt den Inhalt der Datei als String zurück. 

```Clojure 
(def file-contents (read-string "test.txt"))
(println file-contents)
```

Ausgabe: 
```Clojure
This is a test file.
```

Um die Datei in eine Liste von Zeilen zu konvertieren, können wir die Funktion ```line-seq``` verwenden. Diese Funktion nimmt ebenfalls den Dateipfad als Argument, gibt aber eine Liste von Strings zurück, wobei jeder String eine Zeile aus der Datei repräsentiert. 

```Clojure
(def file-lines (line-seq "test.txt"))
(println file-lines)
```

Ausgabe: 
```Clojure
("This is the first line." 
"This is the second line.")
```

## Tiefere Einblicke

Das Lesen von Textdateien ist ein wichtiger Teil der Datenverarbeitung und -manipulation in vielen Programmiersprachen. In Clojure gibt es neben der Funktion ```read-string``` noch andere Möglichkeiten, um Textdateien zu lesen und zu verarbeiten, wie zum Beispiel die Funktion ```slurp``` oder die Bibliothek ```clojure.data.csv```. Diese bieten erweiterte Funktionen und Formate für das Lesen von Textdateien.

## Siehe auch

- [Clojure Dokumentation für read-string](https://clojuredocs.org/clojuredocs.org/clojure.core/read-string)
- [Clojure Data CSV Bibliothek](https://github.com/clojure/data.csv)
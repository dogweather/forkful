---
title:                "Clojure: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man als Programmierer oder Programmiererin temporäre Dateien erstellen würde. Sie können beispielsweise zum Zwischenspeichern von Daten oder als Teil eines größeren Prozesses nützlich sein. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in Clojure temporäre Dateien erstellt und verwendet.

## Wie man temporäre Dateien in Clojure erstellt

Das Erstellen einer temporären Datei in Clojure ist ganz einfach und erfordert nur wenige Zeilen Code. Zuerst müssen Sie das `java.io.File`-Modul importieren:

```Clojure
(require '[clojure.java.io :as io])
```

Als nächstes können Sie die Funktion `with-temp-file` verwenden, um eine temporäre Datei zu erstellen. Diese Funktion erstellt automatisch einen Dateinamen für Sie und gibt dann eine Schreibekannte auf die Datei zurück. Sie können dann die Datei wie jede andere Datei in Clojure verwenden. Hier ist ein Beispiel, das eine temporäre Datei mit dem Inhalt "Hello World!" erstellt und dann den Inhalt der Datei ausgibt:

```Clojure
(with-temp-file [temp-file (io/file "/tmp/")]  ; Die temporäre Datei wird im temporären Verzeichnis erstellt
  (spit temp-file "Hello World!") ; Schreibt "Hello World!" in die Datei
  (slurp temp-file)) ; Gibt den Inhalt der Datei zurück
```

Die Ausgabe dieses Beispiels lautet "Hello World!".

## Eintauchen in die Details

Das Erstellen einer temporären Datei in Clojure ist eigentlich nur eine Abkürzung für die Verwendung von Java-Code unter der Haube. Die Funktion `with-temp-file` verwendet `java.io.File/createTempFile` und `deleteOnExit`, um die temporäre Datei zu erstellen und sie nach dem Beenden des Programms automatisch zu löschen.

Außerdem können Sie der Funktion `with-temp-file` optional einen Präfix und/oder ein Suffix für den Dateinamen übergeben. Zum Beispiel:

```Clojure
(with-temp-file [temp-file (io/file "/tmp/" "foo" ".txt")] ; Präfix "foo" und Suffix ".txt" für den Dateinamen
  (slurp temp-file))
```

Die temporäre Datei heißt in diesem Fall "foo<zufällige Nummer>.txt".

# Siehe auch

- [Clojure Dokumentation für java.io.File](https://clojuredocs.org/clojure.java.io/file)
- [Java Dokumentation für createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Java Dokumentation für deleteOnExit](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#deleteOnExit--)
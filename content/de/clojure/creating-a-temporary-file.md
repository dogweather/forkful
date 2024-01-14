---
title:                "Clojure: Erstellen einer temporären Datei"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine häufige Aufgabe beim Programmieren, die oft übersehen wird. Es ist jedoch wichtig zu wissen, wie man temporäre Dateien erstellt und verwaltet, da sie in verschiedenen Szenarien sehr nützlich sein können. Zum Beispiel könnten Sie eine temporäre Datei verwenden, um Zwischendaten bei der Verarbeitung von großen Datenmengen zu speichern.

## Anleitung

Das Erstellen einer temporären Datei in Clojure ist ganz einfach. Alles, was Sie tun müssen, ist das `clojure.java.io/file`-Modul zu importieren und die Funktion `createTempFile` aufzurufen.

```Clojure
(require '[clojure.java.io :as io])

(def my-temp-file (io/createTempFile "prefix" ".txt"))
```

In diesem Beispiel haben wir die Funktion `createTempFile` mit zwei Parametern aufgerufen. Der erste ist der Präfix für die temporäre Datei und der zweite ist die Dateiendung. Die Funktion gibt eine Instanz von `java.io.File`zurück, die das neu erstellte temporäre File repräsentiert.

Sie können auch optional einen dritten Parameter übergeben, um ein bestimmtes Verzeichnis für die temporäre Datei anzugeben. Wenn Sie den dritten Parameter weglassen, wird die temporäre Datei im Standard-Temp-Verzeichnis Ihres Betriebssystems erstellt.

Die temporäre Datei kann nun verwendet werden, wie jede andere Datei auch. Wir können beispielsweise Informationen in die Datei schreiben und sie dann lesen:

```Clojure
(-> my-temp-file
    io/writer
    (.write "Dies ist ein Beispieltext, der in die temporäre Datei geschrieben wird.")
    .close)

(-> my-temp-file
    io/reader
    .read)
;; => "Dies ist ein Beispieltext, der in die temporäre Datei geschrieben wird."
```

## Tiefentauchen

Wenn Sie eine temporäre Datei erstellen, wird sie im Hintergrund auch von Ihrem Betriebssystem erstellt. Wenn Sie also viele temporäre Dateien erstellen, kann dies Ihre Systemressourcen beeinträchtigen. Um dies zu vermeiden, ist es wichtig, die temporären Dateien zu löschen, sobald sie nicht mehr benötigt werden.

Glücklicherweise können wir dies mit der Funktion `deleteOnExit` aus dem `clojure.java.io`-Modul automatisieren:

```Clojure
(io/deleteOnExit my-temp-file)
```

Durch den Aufruf dieser Funktion wird die temporäre Datei automatisch gelöscht, wenn das Programm beendet wird. Es ist jedoch wichtig zu beachten, dass dies nicht bei einem plötzlichen Programmabsturz funktionieren wird.

## Siehe auch

- [Offizielle Clojure Dokumentation zu temporären Dateien](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/createTempFile)
- [Tutorial zur Arbeit mit Dateien in Clojure](https://www.tutorialspoint.com/clojure/clojure_working_with_files.htm)
- [Ein praktisches Beispiel für die Verwendung von temporären Dateien in Clojure](https://masnun.com/2017/07/24/clojure-using-temporary-files-capability.html)
---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien schreiben bedeutet, Daten in einer einfachen Datei zu speichern, die als Text gelesen werden kann. Programmierer machen das, um Daten dauerhaft zu speichern, Logfiles zu erstellen oder Konfigurationen zu speichern.

## How to:
```Clojure
(with-open [writer (java.io.BufferedWriter. (java.io.FileWriter. "beispiel.txt"))]
  (.write writer "Hallo, Clojure-Leser!"))
```

Das obige Beispiel schreibt den Satz "Hallo, Clojure-Leser!" in eine Datei namens "beispiel.txt".

## Deep Dive
Das Schreiben von Textdateien in Clojure geschieht oft über das Java-Interop, weil Clojure auf der Java Virtual Machine läuft und Java's I/O-APIs nutzt. Alternativen zum `java.io.BufferedWriter` sind zum Beispiel `spit` für den einfachen Gebrauch oder Bibliotheken wie `clojure.java.io` für komplexere Aufgaben. Die Implementierungsdetails variieren je nach Anforderung, können aber Aspekte wie Zeichenkodierung, Pufferung und Fehlerbehandlung umfassen.

## See Also
- [Clojure-Dokumentation zu java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java-Dokumentation zu BufferedWriter](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/io/BufferedWriter.html)
- [Einführung in Clojure](https://clojure.org/guides/getting_started)

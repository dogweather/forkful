---
title:                "Erstellen einer temporären Datei"
html_title:           "Clojure: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ist ein nützliches Konzept in der Programmierung, das es ermöglicht, vorübergehende Dateien zu erstellen, die nur für eine bestimmte Aufgabe oder Sitzung relevant sind. Programmierer nutzen temporäre Dateien, um Daten zu speichern, auf die sie nur vorübergehend zugreifen müssen, oder um Experimente durchzuführen, ohne die ursprüngliche Datei zu verändern. 

## Wie geht's?

### Erstellen einer temporären Datei:
```Clojure
(with-open [f (io/file (java.io.File/createTempFile "prefix" "suffix"))]
   ;;Code zum Bearbeiten der temporären Datei
)
```
Jedes Mal, wenn der Code ausgeführt wird, wird eine neue temporäre Datei mit einem zufälligen Namen in einem vom Betriebssystem dafür vorgesehenen temporären Verzeichnis erstellt.

### Schreiben in eine temporäre Datei:
```Clojure
(with-open [f (io/writer (java.io.File/createTempFile "prefix" "suffix"))]
   (.write f "Dieser Text wird in die temporäre Datei geschrieben!")
)
```
Die Datei wird beim Ausführen dieses Codes automatisch geschlossen, wodurch der temporäre Speicherplatz freigegeben wird.

### Lesen aus einer temporären Datei:
```Clojure
(with-open [f (io/reader (io/resource "tempfile.txt"))]
   (.read f)
)
```
Das oben genannte Beispiel liest den gesamten Inhalt der temporären Datei "tempfile.txt".

## Tiefentauchen

### Historischer Kontext:
Das Konzept der temporären Dateien wurde bereits in den 60er Jahren eingeführt, als Computerspeicher begrenzter und teurer war. Daher war es wichtig, vorübergehende Dateien effizient zu verwalten.

### Alternativen:
Eine alternative Möglichkeit, vorübergehende Daten zu speichern, ist die Verwendung von Speicherobjekten wie Vektoren oder Maps. Dies kann im Gegensatz zur Erstellung einer physischen Datei jedoch mehr Speicherplatz in Anspruch nehmen.

### Implementierungsdetails:
Die Funktion `createTempFile` wird aus der Java-Bibliothek genutzt, um eine temporäre Datei zu erstellen. Sie akzeptiert ein Präfix und ein Suffix für den Dateinamen. Die erstellte Datei wird standardmäßig in dem temporären Verzeichnis gespeichert, das vom Betriebssystem zugewiesen wird.

## Sieh dir auch an:
Offizielle Clojure-Dokumentation zu temporären Dateien: https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/createTempFile
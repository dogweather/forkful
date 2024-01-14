---
title:                "Kotlin: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Manchmal müssen wir in unseren Programmierprojekten temporäre Dateien erstellen. Das können zum Beispiel Zwischenergebnisse sein, die während der Ausführung des Programms abgelegt werden müssen. In diesem Blogbeitrag werde ich Ihnen erklären, wie Sie mit Kotlin ganz einfach temporäre Dateien erstellen können.

## Wie geht das?
Mit Kotlin ist das Erstellen von temporären Dateien sehr unkompliziert. Dafür gibt es die Funktion `createTempFile()` aus der Standardbibliothek. Dabei muss lediglich der Prefix und Suffix der temporären Datei angegeben werden. Hier ein Beispiel:

```Kotlin
val tempFile = createTempFile("data", ".csv")
println(tempFile.path)
```

Die Ausgabe dieses Codes würde wie folgt aussehen:

```
C:\Users\Username\AppData\Local\Temp\data123456.csv
```

Kotlin verwendet standardmäßig das Betriebssystem-Standardverzeichnis für temporäre Dateien. Man kann jedoch auch einen spezifischen Ordner für temporäre Dateien angeben, indem man einen zusätzlichen Parameter `directory` hinzufügt.

```Kotlin
val tempFile = createTempFile("data", ".csv", directory = File("C:\Temp"))
```

Auch das Löschen der temporären Datei ist mit Kotlin kein Problem. Dafür gibt es die Funktion `delete()`.

```Kotlin
tempFile.delete()
```

## Tiefergehende Erklärung
Wenn man genauer betrachtet, was passiert, wenn man eine temporäre Datei erstellt, fällt auf, dass Kotlin im Hintergrund eine eindeutige Datei-ID generiert und diese mit Prefix und Suffix zu einer neuen Datei zusammenfügt. Diese Datei-ID besteht aus der aktuellen Datum- und Uhrzeitangabe sowie einem zufälligen Zahlencode. Dadurch wird sichergestellt, dass jede temporäre Datei eindeutig ist und es nicht zu Namenskonflikten kommt.

## Siehe auch
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [CreateTempFile() Funktion](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Java-Kotlin Vergleich: Temporäre Dateien erstellen](https://www.baeldung.com/kotlin/create-temp-file)
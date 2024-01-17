---
title:                "Erstellen einer temporären Datei"
html_title:           "Kotlin: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Erstellen einer temporären Datei ist ein nützlicher Prozess für Programmierer, besonders wenn sie temporäre Daten während der Ausführung ihres Programms speichern wollen. Es kann auch verwendet werden, um temporäre Dateien für Debugging-Zwecke zu erstellen.

Wie geht's?
Kotlin bietet die Funktion "createTempFile()" an, um eine temporäre Datei zu erstellen. Die Datei wird automatisch in einem temporären Ordner erstellt und hat einen zufällig generierten Namen. Hier ist ein Beispiel:

```Kotlin
val tempFile = createTempFile()
println(tempFile.name)
```
Beispiel Ausgabe: "tmp266662282100316787"

Tiefergehende Informationen
Das Konzept der temporären Dateien wurde bereits in den 60er Jahren eingeführt und war damals ein wichtiger Bestandteil von Betriebssystemen. Heutzutage werden sie hauptsächlich von Programmierern verwendet, um temporäre Daten zu speichern oder Debugging-Aufgaben durchzuführen. Alternativ können Entwickler auch die Funktion "File.createTempFile()" aus der Java Klasse "java.io" verwenden, um temporäre Dateien zu erstellen.

Schau Dir Dies Auch An
- Offizielle Dokumentation für erstellte temporäre Dateien in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html
- Einführung in das Erstellen von temporären Dateien in Kotlin: https://www.baeldung.com/kotlin-temporary-file
- Informationen zu Kotlin's "File" Klasse: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/
---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- /de/kotlin/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:43.511443-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis in Kotlin existiert, beinhaltet die Verifizierung der Präsenz eines Verzeichnisses an einem spezifizierten Pfad. Programmierer führen diese Aufgabe durch, um Fehler zu vermeiden, wie z.B. den Versuch, aus einem nicht existierenden Verzeichnis zu lesen oder in dieses zu schreiben, und so eine reibungslosere Dateibehandlung und Datenverwaltung innerhalb von Anwendungen zu gewährleisten.

## Wie:

Kotlin, das auf der JVM läuft, nutzt die Java File API für Dateioperationen, was die Überprüfung der Existenz von Verzeichnissen geradlinig macht. Hier ist ein einfaches Beispiel:

```kotlin
import java.io.File

fun main() {
    val path = "/pfad/zum/verzeichnis"
    val directory = File(path)

    if (directory.exists() && directory.isDirectory) {
        println("Verzeichnis existiert: $path")
    } else {
        println("Verzeichnis existiert nicht: $path")
    }
}
```
Beispielausgabe, unter der Annahme, dass das Verzeichnis existiert:
```
Verzeichnis existiert: /pfad/zum/verzeichnis
```
Und falls es nicht existiert:
```
Verzeichnis existiert nicht: /pfad/zum/verzeichnis
```

In einem Kotlin-Projekt arbeitet man möglicherweise auch häufig mit Kotlin-spezifischen Bibliotheken oder Frameworks, wie Ktor für Webanwendungen oder kotlinx.coroutines für asynchrone Programmierung. Allerdings ist für die Überprüfung, ob ein Verzeichnis existiert, die standardmäßige Java `File` API, wie gezeigt, typischerweise ausreichend und weit verbreitet aufgrund von Kotlins Interoperabilität mit Java. Für diese spezifische Aufgabe sind keine Drittanbieterbibliotheken erforderlich, was sie zugänglich und unkompliziert für Anfänger macht, die von anderen Programmiersprachen zu Kotlin wechseln.

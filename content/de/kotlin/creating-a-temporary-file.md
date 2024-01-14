---
title:                "Kotlin: Erstellen einer temporären Datei"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Warum
Temporäre Dateien sind nützlich, um temporäre Daten während der Laufzeit eines Programms zu speichern. Sie werden oft verwendet, um Zwischenschritte in einem Prozess zu speichern oder um vorübergehende Einstellungen zu nutzen. Das Erstellen einer temporären Datei in Kotlin ist einfach und kann in verschiedenen Szenarien sehr hilfreich sein.

##Wie erstelle ich eine temporäre Datei in Kotlin
Die Erstellung einer temporären Datei in Kotlin erfordert nur wenige Schritte. Zuerst müssen wir die File-Klasse importieren, um mit Dateien arbeiten zu können. Dann müssen wir in der main-Funktion eine neue Instanz der Klasse File erstellen. Der erste Parameter der File-Klasse ist der Pfad zur temporären Datei. In unserem Beispiel nennen wir die Datei "tempfile.txt". Der zweite Parameter gibt an, dass es sich um eine temporäre Datei handelt, indem wir "null" als Wert angeben.

```Kotlin
import java.io.File

fun main() {
  val tempFile = File("tempfile.txt", null)
}
```

Nachdem wir die Datei erstellt haben, müssen wir sie auch als temporäre Datei markieren, indem wir die dot-tempFile () -Methode aufrufen.

```Kotlin
tempFile.deleteOnExit()
```

Jetzt können wir unserer temporären Datei Daten hinzufügen oder lesen.

```Kotlin
tempFile.writeText("Dies ist eine temporäre Datei.")
println(tempFile.readText())
```

Die Ausgabe des obigen Codes wird sein "Dies ist eine temporäre Datei."

##Tiefergehende Einblicke
Beim Erstellen einer temporären Datei in Kotlin werden verschiedene Faktoren berücksichtigt, wie z. B. der verwendete Betriebssystemtyp und die Verfügbarkeit des Dateisystems. Wenn das Betriebssystem keine temporären Dateien unterstützt oder das Dateisystem voll ist, kann das Erstellen einer temporären Datei fehlschlagen. Es ist auch wichtig zu beachten, dass das Löschen der temporären Datei erst beim Beenden des Programms erfolgt. Daher ist es wichtig, sicherzustellen, dass die Datei ordnungsgemäß geschlossen wird, um Speicherlecks zu vermeiden.

##Siehe auch
- [Kotlin Dokumentation für die File-Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/ "Kotlin Dokumentation für die File-Klasse")
- [Java Dokumentation zur Erstellung von temporären Dateien](https://docs.oracle.com/javase/tutorial/essential/io/tmpFile.html "Java Dokumentation zur Erstellung von temporären Dateien")
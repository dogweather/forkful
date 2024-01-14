---
title:                "Kotlin: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind ein grundlegender Bestandteil der Programmierung. Sie bieten eine einfache und effiziente Möglichkeit, Daten zu speichern und zu organisieren. Durch das Schreiben von Textdateien können Entwickler ihre Programme erweitern und anpassen, um Daten langfristig zu speichern oder bestimmte Aufgaben auszuführen.

## How To

Das Erstellen einer Textdatei in Kotlin ist relativ einfach. Zunächst müssen Sie eine Variable erstellen, die den Namen und Speicherort der Datei enthält. Dann verwenden Sie die Methode "createNewFile()" und übergaben die Variable als Parameter. Anschließend können Sie die Datei mit dem BufferedWriter in Kotlin öffnen und den Inhalt schreiben. Hier ist ein Beispielcode:

 ```Kotlin
// Erstellen einer Datei mit dem Namen "Textdatei.txt" 
 val datei = File("Textdatei.txt")
// Erstellen einer neuen Datei 
 datei.createNewFile()
 // Öffnen der Datei mit BufferedWriter 
 val schreiber = BufferedWriter(FileWriter(datei))
// Schreiben von Inhalten 
 schreiber.write("Dies ist ein Beispieltext für unsere Textdatei.")
 // Schließen der Datei 
 schreiber.close()
```
Nachdem Sie die Datei erstellt und den Inhalt geschrieben haben, wird sie an dem von Ihnen angegebenen Speicherort gespeichert. In diesem Beispiel würden Sie die Datei mit dem Namen "Textdatei.txt" im selben Ordner finden, in dem sich Ihr Kotlin-Programm befindet.

Die Ausgabe der Textdatei würde wie folgt aussehen:
Dies ist ein Beispieltext für unsere Textdatei.

## Deep Dive

Es gibt auch Möglichkeiten, spezifische Informationen in eine Textdatei zu schreiben. Mit der Methode "append()" können Sie Inhalte an eine bereits vorhandene Datei anhängen. Wenn Sie eine große Datenmenge haben, können Sie auch den BufferedReader verwenden, um die Daten zeilenweise zu lesen. Mit der Methode "delete()" können Sie eine Textdatei vollständig löschen.

## Siehe auch

- [Offizielle Dokumentation von Kotlin für die Verwendung von Textdateien](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Ein Tutorial zur Verwendung von Textdateien in Kotlin](https://www.raywenderlich.com/783130-text-files-in-kotlin-i-o-and-strings-getting-started)
- [Weiterführende Informationen zur Verwendung von Textdateien in der Programmierung](https://www.codecademy.com/articles/file-extensions)
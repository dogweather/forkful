---
title:    "Kotlin: Erstellen einer temporären Datei"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es viele verschiedene Aufgaben, die Entwickler täglich bewältigen müssen. Eine davon ist die Erstellung von temporären Dateien. Warum sollte man sich überhaupt die Mühe machen, temporäre Dateien zu erstellen? Hier erfährst du es.

## Wie man temporäre Dateien in Kotlin erstellt

Das Erstellen von temporären Dateien in Kotlin ist ein einfacher Prozess. Zunächst müssen wir die `createTempFile()` Methode aufrufen, die Teil der `java.io.File` Klasse ist. Wir können dieser Methode zwei Parameter übergeben: einen optionalen Dateinamen und eine optionale Dateiendung. Wenn wir keinen Namen oder keine Endung angeben, generiert die Methode automatisch einen einzigartigen Dateinamen für uns.

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
```

Wenn wir nun das erstellte `tempFile` Objekt ausgeben, sehen wir den vollständigen Pfad der temporären Datei.

```Kotlin
println(tempFile.getAbsolutePath())
```

Die Ausgabe würde etwa wie folgt aussehen:

```
/var/folders/15/vfx06kgd7731822hxxxfq8mw0000gn/T/temp123456789.txt
```

## Tiefergehende Informationen zur Erstellung von temporären Dateien

Jetzt, da wir wissen, wie man temporäre Dateien in Kotlin erstellt, lassen uns einen Blick auf einige weitere wichtige Details werfen. Die `createTempFile()` Methode erstellt automatisch eine temporäre Datei im entsprechenden System-Temp-Verzeichnis. Dieses Verzeichnis kann je nach Betriebssystem variieren, aber wir können immer auf den Pfad zugreifen, indem wir die `System.getProperty("java.io.tmpdir")` Methode aufrufen.

Wenn wir möchten, können wir auch eine benutzerdefinierte temporäre Datei in einem bestimmten Verzeichnis erstellen, indem wir der `createTempFile()` Methode ein zusätzliches Argument übergeben, das den Speicherort angibt. Außerdem können wir auch die `deleteOnExit()` Methode aufrufen, um sicherzustellen, dass die temporäre Datei automatisch gelöscht wird, sobald das Programm beendet wird.

## Siehe auch

Wenn du mehr über die Arbeit mit temporären Dateien in Kotlin erfahren möchtest, schau dir diese hilfreichen Ressourcen an:

- Offizielle Dokumentation für `java.io.File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html
- Tutorial zur Arbeit mit temporären Dateien in Kotlin: https://www.baeldung.com/kotlin-temporary-file
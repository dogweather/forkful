---
title:                "Java: Erstellen einer temporären Datei"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es oft die Notwendigkeit, temporäre Dateien zu erstellen. Diese temporären Dateien dienen dazu, Daten zwischenzuspeichern und zu verarbeiten, bevor sie in eine dauerhafte Datei geschrieben werden. Sie können auch nützlich sein, um komplexe Berechnungen oder Prozesse aufzuteilen und somit die Gesamtlaufzeit eines Programms zu verbessern.

# Wie erstellt man temporäre Dateien in Java

Die Erstellung einer temporären Datei in Java ist relativ einfach. Es gibt verschiedene Möglichkeiten, dies zu erreichen, aber eine gängige Methode ist die Verwendung der Klasse "java.io.File". Im folgenden Beispiel verwenden wir diese Klasse, um eine temporäre Datei mit dem Namen "example.txt" zu erstellen und einige Daten darin zu speichern:

```Java
// Erstellt eine temporäre Datei im Standard-Temp-Verzeichnis mit dem Präfix "example"
File tempFile = File.createTempFile("example", ".txt");

// Schreibt Daten in die temporäre Datei
BufferedWriter writer = new BufferedWriter(new FileWriter(tempFile));
writer.write("Dies ist eine Beispiel-Temp-Datei");
writer.close();
```

Das Ergebnis dieses Codes wird eine temporäre Datei mit dem Namen "example123456789.txt" sein, wobei die Zahlen je nach System unterschiedlich sein können. Diese Datei wird im Standard-Temp-Verzeichnis des Systems erstellt, das für viele Betriebssysteme üblicherweise "C:\Temp" oder "/tmp" ist.

# Tiefergehende Informationen zur Erstellung temporärer Dateien

Es gibt noch weitere Möglichkeiten, temporäre Dateien in Java zu erstellen und zu verarbeiten. Eine davon ist die Verwendung der Klasse "java.nio.file.Files", die eine Reihe von Methoden enthält, die das Arbeiten mit temporären Dateien erleichtern. In der offiziellen Java-Dokumentation finden Sie eine ausführliche Beschreibung dieser Klasse und ihrer Verwendung.

Ein wichtiger Punkt bei der Arbeit mit temporären Dateien ist es, sicherzustellen, dass sie nach Verwendung gelöscht werden. Wenn dies nicht explizit im Code ausgeführt wird, können temporäre Dateien auf dem System verbleiben und Platz verschwenden. Um dies zu vermeiden, bietet Java eine Methode namens "deleteOnExit()", die die Datei beim Beenden des Programms löscht. Jedoch ist es empfehlenswert, die temporären Dateien selbstständig im Code zu löschen, um sicherzustellen, dass sie nicht unnötig lange bestehen bleiben.

# Siehe auch

- Offizielle Java-Dokumentation zur Klasse "java.nio.file.Files": https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Tutorial zur Arbeit mit temporären Dateien in Java: https://www.baeldung.com/java-temporary-files
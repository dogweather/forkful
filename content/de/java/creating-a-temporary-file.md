---
title:                "Java: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit der Erstellung von temporären Dateien in der Java-Programmierung beschäftigen? Nun, temporäre Dateien sind nützlich, wenn man Daten temporär speichern möchte, ohne die Notwendigkeit einer dauerhaften Speicherung zu haben. Sie werden oft verwendet, um Zwischenergebnisse in einem Programm zu speichern oder um Dateien zu generieren, die später gelöscht werden sollen.

## Wie geht es
Um eine temporäre Datei in Java zu erstellen, gibt es mehrere Schritte, die man befolgen muss. Zuerst muss man das `File`-Objekt erstellen und den Dateinamen sowie den Ort, an dem die Datei erstellt werden soll, angeben. Dann muss man dem `File`-Objekt mitteilen, dass es sich um eine temporäre Datei handelt, indem man die Methode `createTempFile()` aufruft. Dies erstellt die tatsächliche Datei auf dem System.

```Java
File tempFile = new File("temp.txt"); // Erstellt ein File-Objekt mit dem Dateinamen "temp.txt"
tempFile = File.createTempFile("temp", ".txt"); // Markiert die Datei als temporäre Datei

// Wir können nun auf die Datei zugreifen und ihre Eigenschaften ändern
tempFile.setWritable(true); // Setzt die Datei auf beschreibbar
tempFile.setReadOnly(); // Setzt die Datei auf nur lesbar
```

Man kann auch optional den Präfix und Suffix für die temporäre Datei angeben, indem man zwei zusätzliche Parameter in der Methode `createTempFile()` verwendet.

```Java
tempFile = File.createTempFile("temp", ".txt", "/Users/Meine Dateien"); // Erstellt eine temporäre Datei mit dem Präfix "temp" und dem Suffix ".txt" im Ordner "Meine Dateien".
```

Die `createTempFile()`-Methode gibt ein `File`-Objekt zurück, das auf die tatsächliche temporäre Datei auf dem System verweist. Man kann nun wie gewohnt auf die Datei zugreifen und sie verwenden.

## Tiefergehende Informationen
Es ist wichtig zu beachten, dass temporäre Dateien automatisch gelöscht werden, wenn das Programm beendet wird. Man kann jedoch auch manuell angeben, dass die Datei sofort nach dem Beenden des Programms gelöscht werden soll, indem man die `deleteOnExit()`-Methode aufruft.

```Java
tempFile.deleteOnExit(); // Löscht die Datei, sobald das Programm beendet wird.
```

Außerdem können temporäre Dateien auch andere Eigenschaften wie Lese- und Schreibberechtigungen haben, je nach den Berechtigungen, die dem `File`-Objekt gegeben werden.

## Siehe auch
* Java File-Klasse Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
* Java Tutorial zu temporären Dateien: https://www.baeldung.com/java-temporary-files
---
title:    "Java: Erstellen einer temporären Datei"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man als Java-Programmierer temporäre Dateien erstellen möchte. Beispielsweise können sie verwendet werden, um vorübergehende Daten zu speichern, die nicht dauerhaft im System gespeichert werden müssen. Oder sie können als Zwischenspeicher für Daten verwendet werden, die später in eine permanente Datei geschrieben werden sollen. Kurz gesagt, temporäre Dateien sind nützlich, um effizienter mit Daten umzugehen.

## Wie es gemacht wird

Das Erstellen einer temporären Datei in Java ist relativ einfach. Es gibt verschiedene Möglichkeiten, je nachdem, welches JDK (Java Development Kit) verwendet wird. Im Folgenden wird ein Beispiel mit dem aktuellen JDK 11 gezeigt:

```Java
// Erstelle eine temporäre Datei im Standard-Temp-Verzeichnis
File tempFile = File.createTempFile("demo", ".txt");

// Schreibe Daten in die temporäre Datei
BufferedWriter writer = new BufferedWriter(new FileWriter(tempFile));
writer.write("Dies ist ein Beispieltext.");
writer.close();

// Lesen und Ausgabe der Daten aus der temporären Datei
BufferedReader reader = new BufferedReader(new FileReader(tempFile));
String line = reader.readLine();
System.out.println("Inhalt der temporären Datei: " + line);

// Löschen der temporären Datei
tempFile.delete();
```

Das obige Beispiel erstellt eine temporäre Textdatei im Standard-Temp-Verzeichnis des Betriebssystems und schreibt einen Beispieltext hinein. Anschließend wird der Inhalt der Datei ausgelesen und in der Konsole ausgegeben. Am Ende wird die Datei wieder gelöscht.

## Tiefer Einblick

Das `File`-Objekt, das mit der `createTempFile()`-Methode erstellt wird, enthält zusätzliche Informationen über die erstellte temporäre Datei. Beispielsweise kann mit der Methode `getAbsolutePath()` der vollständige Pfad zur Datei abgerufen werden.

Es ist auch möglich, ein eigenes temporäres Verzeichnis anzugeben, in dem die Datei erstellt werden soll, indem man den zweiten Parameter der `createTempFile()`-Methode verwendet. Außerdem können optionale Dateiattribute wie Lese- oder Schreibberechtigungen mit der `setExecutable()`-Methode gesetzt werden.

Es ist wichtig zu beachten, dass temporäre Dateien nicht automatisch gelöscht werden. Sie müssen vom Programmierer manuell gelöscht werden, wenn sie nicht mehr benötigt werden. Dies kann mit der `delete()`-Methode des `File`-Objekts erfolgen.

## Siehe auch

- [Dokumentation zur `File`-Klasse von Oracle](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Beispiel für das Arbeiten mit temporären Dateien von Baeldung](https://www.baeldung.com/java-temporary-files)
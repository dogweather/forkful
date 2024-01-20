---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ist ein Vorgang, bei dem eine Datei mit zufälliger oder eindeutiger Bezeichnung erzeugt wird. Programmierer nutzen temporäre Dateien meistens zwischen Speicherungen, wenn sie eine Menge von Daten verarbeiten, die speziell für den aktuellen Lauf des Programms relevant sind. 

## So wird's gemacht:

Java hat eine eigene Methode zum Erstellen von temporären Dateien in der `java.io.File` Klasse.

```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            File temp = File.createTempFile("myTempFile", ".txt");
            System.out.println("Temp file created : " + temp.getAbsolutePath());
            } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Ausfahrt:

```
Temp file created : C:\Users\User\AppData\Local\Temp\myTempFile7764858948535314628.txt
```
## Tiefergehende Infos

Historisch gesehen wurden temporäre Dateien in den frühen Tagen des Programmierens verwendet, um Platz auf teuren Festplatten zu sparen. Heutzutage sind sie hauptsächlich geblieben, um Daten zwischen den Läufen eines Programms zu speichern, Raum für grosse Datenmengen zu schaffen, oder einfach zur Fehlerbehebung.

Es gibt Alternativen zur Verwendung von temporären Dateien, darunter Datenbanken und In-Memory-Datenspeicher, aber diese können teurer und komplizierter in der Implementierung sein.

Java's Methode `createTempFile()` erstellt eine temporäre Datei in dem Verzeichnis, das vom System-Property `java.io.tmpdir` angegeben wird. Sie erstellt auch einen einzigartigen Dateinamen, so dass Sie sich keine Sorgen über Kollisionen mit anderen Dateinamen machen müssen.

## Siehe auch

Hier sind einige nützliche Links zum Thema temporäre Dateien:

- Das [Java API-Dokument](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) für die File Klasse.


Vergessen Sie nicht, Ihre temporären Dateien zu löschen, wenn Sie mit ihnen fertig sind!
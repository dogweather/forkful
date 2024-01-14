---
title:                "Java: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Schreiben von Textdateien ist ein grundlegender Bestandteil der Programmierung in Java. Diese Art von Dateien ermöglicht es dem Programmierer, Daten dauerhaft zu speichern und zu verarbeiten. Durch das Schreiben von Textdateien haben Sie die Möglichkeit, Informationen zwischen Programmläufen zu übertragen und sie für spätere Analyse und Verarbeitung zu speichern.

## Wie man eine Textdatei schreibt
Das Schreiben einer Textdatei in Java ist einfach und erfordert nur wenige Zeilen Code. Zunächst müssen Sie die Klasse FileWriter importieren und eine Instanz davon erstellen. Dann können Sie die gewünschte Datei öffnen und einen BufferedWriter erstellen, um den Text in die Datei zu schreiben. Nach dem Schreiben können Sie den BufferedWriter schließen, um sicherzustellen, dass die Datei korrekt gespeichert wird. Im folgenden Beispiel können Sie sehen, wie man eine Textdatei mit ein paar Zeilen Text schreibt:

```Java
import java.io.*;

public class TextDateiSchreiben {
    public static void main(String[] args) throws IOException {
        FileWriter datei = new FileWriter("meineDatei.txt");
        BufferedWriter out = new BufferedWriter(datei);

        out.write("Dies ist ein Beispieltext.");
        out.newLine();
        out.write("Hier können Sie weitere Texte hinzufügen.");

        out.close();
    }
}
```

Die oben genannten Codezeilen werden eine Datei namens "meineDatei.txt" im gleichen Verzeichnis wie das Programm erstellen und den Text an die Datei anhängen. Wenn Sie die Datei öffnen, sollten Sie den gleichen Text sehen, den Sie im Code eingegeben haben.

## Tiefer Einblick
Das Schreiben von Textdateien in Java erlaubt es auch, Variablen und benutzerdefinierte Datenstrukturen in die Datei zu schreiben. Einige der wichtigsten Funktionen, die beim Arbeiten mit Textdateien zu beachten sind, sind die Verwendung von `try-catch` Blöcken, um Fehler beim Schreiben der Datei abzufangen, und die Verwendung des `close()` Befehls, um sicherzustellen, dass die Datei korrekt geschlossen und gespeichert wird.

Es ist auch wichtig zu beachten, dass beim Schreiben von Textdateien die Daten als Bytes codiert werden müssen. Dies kann durch die Verwendung des `getBytes()` Befehls erreicht werden, der die Daten in ein Byte-Array konvertiert, das dann in die Datei geschrieben werden kann.

## Siehe auch
Weitere Informationen zum Schreiben von Textdateien in Java finden Sie in der offiziellen Java-Dokumentation unter: 
- https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html
- https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html
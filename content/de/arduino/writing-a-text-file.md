---
title:                "Das Schreiben einer Textdatei"
html_title:           "Arduino: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ist ein Prozess, bei dem ein Programmierer Text in eine Datei auf einem Computer oder Mikrocontroller schreibt. Dies kann hilfreich sein, um Daten zu speichern oder Protokolle von Programmen zu erstellen.

## Wie geht's?
Das Schreiben einer Textdatei in Arduino ist einfach und kann mit der `File`-Bibliothek durchgeführt werden. Zunächst erstellen wir ein Dateiobjekt und öffnen dann die Datei mit dem Speicherort und dem gewünschten Dateinamen. Anschließend können wir den Text mit der `print()`-Funktion in die Datei schreiben. Zum Schluss müssen wir die Datei schließen, damit die Änderungen gespeichert werden. Hier ist ein Beispielcode:

```
// Dateiobjekt erstellen
File myFile;

// Datei öffnen
myFile = SD.open("MeinText.txt", FILE_WRITE);

// Text in die Datei schreiben
myFile.print("Dies ist ein Beispieltext.");

// Datei schließen
myFile.close();
```

Das Resultat wird eine Textdatei mit dem Namen "MeinText.txt" mit dem Inhalt "Dies ist ein Beispieltext." sein.

## Tiefentauchen
Das Schreiben von Textdateien ist eine gängige Funktion in den meisten Programmiersprachen und wird in der Regel verwendet, um Daten langfristig zu speichern. Alternativ kann auch die `Serial`-Bibliothek verwendet werden, um Text auf dem seriellen Monitor anzuzeigen. Die Implementierung kann je nach Mikrocontroller variieren, aber die grundlegenden Schritte bleiben gleich.

## Siehe auch
- Offizielle Arduino-Referenz für die `File`-Bibliothek: https://www.arduino.cc/en/Reference/File
- Tutorial zum Schreiben einer Textdatei in Arduino: https://www.arduino.cc/en/Tutorial/WriteTextFile
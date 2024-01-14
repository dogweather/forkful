---
title:                "Arduino: Eine Textdatei schreiben"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Der Prozess des Schreibens einer Textdatei kann in der Programmierung sehr nützlich sein, zum Beispiel um Daten zu speichern oder um eine Benutzeroberfläche zu erstellen. In diesem Artikel werden wir uns ansehen, wie man mit Hilfe von Arduino eine Textdatei erstellen und bearbeiten kann.

## Wie geht das?

Um eine Textdatei auf einem Arduino-Board zu erstellen, müssen wir zunächst die SD-Bibliothek importieren und eine SD-Karte in das Board einlegen. Dann können wir mit der `File`-Klasse eine neue Datei erstellen und sie mit Inhalte füllen. Schauen wir uns dazu ein Beispiel an:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  // SD-Bibliothek initialisieren
  SD.begin(4);
  // Neue Datei mit dem Namen "text.txt" erstellen
  myFile = SD.open("text.txt", FILE_WRITE);
  // Daten in die Datei schreiben
  myFile.println("Hallo Welt!");
  // Datei schließen
  myFile.close();
}

void loop() {

}
```

In diesem Beispiel haben wir die `myFile`-Variable verwendet, um die erstellte Datei zu verwalten. Mit der `println()` Funktion können wir einen Text in die Datei schreiben und mit `close()` schließen wir die Datei. Durch diesen einfachen Prozess können wir eine Textdatei auf unser Arduino-Board schreiben.

## Tiefer eintauchen

Nun, da wir wissen, wie man eine Textdatei erstellt, können wir uns etwas tiefer damit beschäftigen und mehrere Dateien erstellen, anstatt nur eine. Dazu können wir eine `for`-Schleife verwenden, um die Dateinamen zu iterieren und somit mehrere Dateien zu erstellen. Hier ist ein Beispiel:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  // SD-Bibliothek initialisieren
  SD.begin(4);
  // Schleife durchlaufen und Dateinamen von 1-5 erstellen
  for (int i = 1; i <= 5; i++) {
    // Datei mit entsprechendem Namen erstellen
    myFile = SD.open("text" + String(i) + ".txt", FILE_WRITE);
    // Daten in die jeweilige Datei schreiben
    myFile.println("Text in Datei " + String(i));
    // Datei schließen
    myFile.close();
  }
}

void loop() {

}
```

In diesem Beispiel nutzen wir die `String()`-Funktion, um den Dateinamen dynamisch zu erstellen. Wir können auch Dateien lesen und bearbeiten, indem wir die `SD.open()` Funktion mit dem Parameter `FILE_READ` verwenden und dann `myFile.read()` verwenden, um die Daten auszulesen. Es besteht auch die Möglichkeit, Dateien zu löschen, indem wir die `SD.remove()` Funktion verwenden.

## Siehe auch

- [Die offizielle Arduino-Website](https://www.arduino.cc/)
- [SD-Bibliothek Referenz](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: Writing files to an SD card](https://www.arduino.cc/en/Tutorial/WriteToSDCard)
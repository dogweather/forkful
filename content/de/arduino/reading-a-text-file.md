---
title:    "Arduino: Eine Textdatei lesen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist ein wichtiger Teil der Datenverarbeitung in der heutigen Welt. Es ermöglicht uns, große Mengen von Daten zu speichern und einfach darauf zuzugreifen, um unsere Projekte zu verbessern und effizienter zu gestalten. In diesem Blogbeitrag werden wir uns mit der Programmierung eines Arduinos befassen, um Textdateien zu lesen und die Informationen daraus zu nutzen.

## Wie geht's

Um Textdateien mit einem Arduino zu lesen, müssen wir zunächst die entsprechenden Bibliotheken importieren. Dazu fügen wir folgende Zeilen am Anfang unseres Codes ein:

```Arduino
#include <SD.h>
#include <SPI.h>
```

Als nächstes müssen wir eine SD-Karte in den richtigen Anschluss des Arduino-Boards einlegen. Anschließend können wir mit dem Öffnen der Textdatei beginnen. Dazu verwenden wir die Funktion ```SD.open()``` und übergeben den Namen der Datei sowie den gewünschten Modus (lesen, schreiben, anhängen). Beispiel: 

```Arduino
File myFile = SD.open("beispiel.txt", FILE_READ);
```

Sobald die Datei erfolgreich geöffnet wurde, können wir mit der Funktion ```myFile.read()``` den Inhalt der Datei Zeichen für Zeichen auslesen und in einer Variablen speichern. Hier ein Beispiel, in dem wir die Zeichen nacheinander auslesen und auf dem Seriellen Monitor ausgeben:

```Arduino
while (myFile.available()) {
    char c = myFile.read();
    Serial.print(c);
}
```

Diese Schleife liest die Datei, solange noch Zeichen vorhanden sind. Dabei wird jedes Zeichen in der Variablen ```c``` gespeichert und anschließend über ```Serial.print()``` ausgegeben.

## Tiefer tauchen

Es gibt viele zusätzliche Funktionen, die beim Lesen von Textdateien hilfreich sein können, wie zum Beispiel das Springen zu bestimmten Stellen in der Datei. Dabei kann die Funktion ```myFile.seek()``` verwendet werden, die zwei Parameter entgegennimmt - die Position, zu der gesprungen werden soll, und der Bezugspunkt (Anfang, aktuelle Position oder Ende der Datei).

Zusätzlich kann auf ähnliche Weise auch auf Binärdateien zugegriffen werden, indem der Modus in der ```SD.open()``` Funktion auf ```FILE_READ_BIN``` geändert wird, und der Inhalt mit der Funktion ```myFile.readBytes()``` gelesen wird.

## Siehe auch

- [Arduino SD Bibliothek Referenz](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: Dateien auf einer SD-Karte lesen und schreiben](https://www.circuitbasics.com/how-to-write-and-read-from-an-sd-card-with-an-arduino/)
- [Arduino Beispielcode für das Lesen von Textdateien](https://www.arduino.cc/en/Tutorial/ReadWrite)
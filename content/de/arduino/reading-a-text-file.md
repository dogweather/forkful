---
title:                "Arduino: Eine Textdatei lesen"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen und Verarbeiten von Textdateien ist ein wichtiger Bestandteil der Programmierung mit Arduino. Es ermöglicht die Verwendung von externen Daten in Projekten und eröffnet viele Möglichkeiten für Kreativität und Innovation. In diesem Blog-Beitrag werden wir uns ansehen, wie man eine Textdatei mit Arduino liest und verarbeitet.

## Wie geht's

Um eine Textdatei mit Arduino zu lesen, benötigen wir die `SD` Bibliothek, die Teil der Standardbibliothek von Arduino ist. Diese Bibliothek ermöglicht die Kommunikation mit einer SD-Karte, auf der wir unsere Textdatei speichern können.

Hier ist ein Beispielcode, der eine Textdatei mit dem Namen "data.txt" auf einer SD-Karte liest und ihren Inhalt in der Seriellen Monitor ausgibt:

```Arduino
#include <SD.h>

File myFile; // Variable für die Textdatei
int pinChipSelect = 10; // Pin, an dem die SD-Karte angeschlossen ist

void setup() {
  Serial.begin(9600);
  
  // Initialisiere die SD-Karte und öffne die Textdatei
  SD.begin(pinChipSelect);
  myFile = SD.open("data.txt");
}

void loop() {
  // Lese eine Zeile aus der Textdatei
  String data = myFile.readStringUntil('\n');
  
  // Gib die gelesene Zeile im Seriellen Monitor aus
  Serial.println(data);
}

```

Die Ausgabe im Seriellen Monitor wird wie folgt aussehen:

```
Dies ist Zeile 1
Hier ist die zweite Zeile
Und hier kommt die dritte
```

## Tiefentauchen

Es gibt mehrere nützliche Funktionen, die in der `SD` Bibliothek enthalten sind und die es uns ermöglichen, Textdateien in verschiedene Formate und mit verschiedenen Methoden zu lesen. Hier sind einige davon:

- `readString()` - liest die nächste Zeile der Textdatei und gibt sie als Zeichenkette zurück.
- `readInt()` - liest die nächste Zeile der Textdatei und gibt sie als Integer-Wert zurück.
- `parseFloat()` - liest die nächste Zeile der Textdatei und gibt sie als Gleitkommazahl zurück.
- `seek()` - ermöglicht es uns, zu einer bestimmten Position in der Textdatei zu springen.

Es gibt noch viele weitere Funktionen, die du ausprobieren kannst. Eine vollständige Liste findest du in der [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD).

## Siehe auch

- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: How to Read and Write to a Text File with Arduino](https://randomnerdtutorials.com/how-to-write-and-read-from-arduino-sd-card/)
- [SD Library Examples](https://www.arduino.cc/en/Tutorial/LibraryExamples/SDReadWrite)
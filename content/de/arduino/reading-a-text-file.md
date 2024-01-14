---
title:    "Arduino: Das Lesen einer Textdatei"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Lesen und Bearbeiten von Textdateien ist ein wichtiger Teil der Arduino Programmierung. Es ermöglicht uns, Daten zu speichern und wiederzuverwenden, um komplexe Programme zu erstellen. Lesen Sie weiter, um zu erfahren, wie Sie Textdateien in Ihrer Arduino Programmierung nutzen können.

## Wie
Um eine Textdatei in Arduino zu lesen, müssen wir zunächst die Datei auf unserem Computer oder Laptop erstellen. Wir können dies entweder mit einer Texteditor-Software wie Notepad oder direkt in der Arduino IDE tun. Anschließend laden wir die Datei auf unseren Arduino. Das folgende Beispiel liest eine Textdatei mit dem Namen "beispiel.txt" von der SD-Karte des Arduino und gibt den Inhalt auf dem Seriellen Monitor aus.

```Arduino
#include <SPI.h>
#include <SD.h>

File textFile;

void setup() {
  // Initialisierung der SD-Karte
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("SD-Karte kann nicht gelesen werden!");
    return;
  }

  // Öffnen der Textdatei
  textFile = SD.open("beispiel.txt");
  if (textFile) {
    // Ausgabe des Inhalts auf dem Seriellen Monitor
    while (textFile.available()) {
      Serial.write(textFile.read());
    }
    textFile.close();
  } else {
    Serial.println("Fehler beim Öffnen der Datei!");
  }
}

void loop() {
  // nichts weiteres zu tun
}

```

Der Inhalt der Textdatei "beispiel.txt" könnte zum Beispiel folgendermaßen aussehen:

```
Hallo! Dies ist eine Textdatei.
Sie kann von der SD-Karte gelesen werden.
Das ist großartig!
```

Wenn wir nun den Code hochladen und den Seriellen Monitor öffnen, sehen wir den Text aus der Datei angezeigt werden.

```
Hallo! Dies ist eine Textdatei.
Sie kann von der SD-Karte gelesen werden.
Das ist großartig!
```

## Deep Dive
Nun, da wir wissen, wie man eine Textdatei liest, können wir auch andere Aktionen wie das Schreiben oder Löschen von Inhalten aus der Datei durchführen. Unter Verwendung der Funktionen `textFile.write()` und `textFile.println()` können wir den Inhalt einer Variablen oder eines Wertes in die Datei schreiben. Zum Beispiel können wir den Inhalt unseres Sensorwertes in die Datei schreiben, um eine Art Protokoll zu erstellen. Wir können auch die SD-Karte formatieren oder bestimmte Teile der Datei löschen. Es gibt viele Möglichkeiten, Textdateien in der Arduino Programmierung zu nutzen, und es lohnt sich, damit zu experimentieren.

## Siehe auch
- [Arduino Dokumentation](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: Reading and Writing Text Files on an SD Card with Arduino](https://learn.adafruit.com/adafruit-data-logger-shield/using-the-real-time-clock) (auf Englisch)
- [Tutorial: Logging Data to a Text File with Arduino](https://www.instructables.com/id/Arduino-Logging-Data-to-a-Text-File-on-a-PC/) (auf Englisch)
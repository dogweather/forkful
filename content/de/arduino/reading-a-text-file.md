---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Das Lesen von Textdateien in Arduino

*Note: This article is in Informal German.

## Was & Warum?

Das Lesen von Textdateien ist ein Prozess, bei dem Daten aus einer Datei abgerufen werden. Programmierer machen dies oft, um voreingestellte Werte zu laden, Protokolle zu speichern oder zur Verarbeitung von Daten.

## So wird's gemacht:

Ein Beispiel zeigt das Lesen einer Textdatei mit Arduino. Beachten Sie, dass ein SD-Kartenleser erforderlich ist.

```Arduino
#include <SPI.h>
#include <SD.h>

File meineDatei;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisierung fehlgeschlagen!");
    return;
  }
  
  meineDatei = SD.open("test.txt");
  if (meineDatei) {
    while(meineDatei.available()) {
      Serial.write(meineDatei.read());
    }
    meineDatei.close();
  } else {
    Serial.println("Fehler beim Öffnen der Datei");
  }
}

void loop() {

}
```

Wenn "Hallo Welt" in der Datei `test.txt` gespeichert ist, wird die Ausgabe:

```
Hallo Welt
```

## Tiefgreifende Details:

Historisch gesehen wurde das Lesen von Dateien in Arduino erstmals mit der Implementierung der SD-Bibliothek möglich. Alternativ kann das Lesen von Dateien auch mit Ethernet- oder WiFi-Shields und einer Verbindung zu einem Server ermöglicht werden. Der hier vorgestellte Ansatz verwendet die `read()` -Methode der `File` -Objekte aus der SD-Bibliothek. Diese Methode gibt ein Byte der Datei zurück und bewegt den "Cursor" um ein Byte weiter.

## Siehe auch:

Besuchen Sie diese Ressourcen, um mehr zu erfahren:

1. [Arduino SD Bibliotheksdokumentation](https://www.arduino.cc/en/Reference/SD)
2. [Dateizugriff mit Arduino](https://learn.adafruit.com/adafruit-data-logger-shield/using-the-real-time-clock-3)
3. [Arduino Datei-Lesen-Tutorials](https://create.arduino.cc/projecthub/projects/tags/file+read)
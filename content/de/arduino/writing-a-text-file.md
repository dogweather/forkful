---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ermöglicht es, Daten dauerhaft zu speichern. Programmierer nutzen dies, um Einstellungen, Nutzerdaten oder Protokolle zu bewahren.

## How to:
```Arduino
#include <SD.h>

File meineDatei;

void setup() {
  // SD-Karte auf Pin 4 initialisieren
  SD.begin(4);
  
  // Datei "example.txt" zum Schreiben öffnen
  meineDatei = SD.open("example.txt", FILE_WRITE);
  
  // Text in die Datei schreiben, wenn sie geöffnet ist
  if (meineDatei) {
    meineDatei.println("Hallo Welt!");
    // Datei schließen
    meineDatei.close();
  } else {
    // Wenn die Datei nicht geöffnet werden konnte, Fehlermeldung ausgeben
    Serial.println("Fehler beim Öffnen der Datei");
  }
}

void loop() {
  // Nichts zu tun hier.
}
```
**Beispielausgabe:**
`Hallo Welt!` gespeichert in `example.txt` auf der SD-Karte.

## Deep Dive
Das Schreiben von Dateien auf einer SD-Karte ist ein Prozess mit langer Tradition in der Computergeschichte. Alternativen wie EEPROM-Speicher sind ebenfalls verbreitet, bieten jedoch weniger Speicherplatz. Wichtig beim Schreiben von Dateien ist der Umgang mit Fehlern und das ordnungsgemäße Schließen der Datei, um Datenverlust zu vermeiden.

## See Also:
- Arduino SD Library Dokumentation: https://www.arduino.cc/en/Reference/SD
- Tutorial zum Umgang mit Dateien auf der SD-Karte: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- EEPROM-Speicher in Arduino verwenden: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite

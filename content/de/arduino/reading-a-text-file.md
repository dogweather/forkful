---
title:                "Arduino: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine wichtige Fähigkeit für jeden Arduino-Programmierer. Es ermöglicht die Kommunikation mit anderen Geräten oder das Speichern von Daten für spätere Verwendung.

## Wie geht's

Das Lesen einer Textdatei mit Arduino erfordert drei Schritte:

1. Öffnen der Textdatei: Verwende die `SD.open()` Funktion, um die Textdatei zu öffnen und einen Dateizeiger zu erhalten.
2. Lesen der Datei: Verwende die `SD.read()` Funktion, um den Inhalt der Datei auszulesen. Speichere die gelesenen Daten in einer Variable.
3. Schließen der Datei: Verwende die `SD.close()` Funktion, um die Datei wieder zu schließen.

Um einen einfachen Beispielcode auszuführen, stelle sicher, dass du eine SD-Karte an deinen Arduino angeschlossen hast und die SD-Kartenbibliothek eingebunden hast:

```Arduino
#include <SD.h>

File myFile; // Erstelle eine Dateivariable

void setup() {
  Serial.begin(9600); // Verbinde den seriellen Monitor
  SD.begin(4); // Initialisiere die SD-Karte auf Pin 4
  myFile = SD.open("test.txt"); // Offne die Datei "test.txt"
  
  // Überprüfe, ob die Datei geöffnet wurde
  if (myFile) {
    Serial.println("Successfully opened file!");
    
    // Lese den Inhalt Zeile für Zeile
    while (myFile.available()) {
      Serial.write(myFile.read()); //Gib den gelesenen Inhalt aus
    }
    
    myFile.close(); // Schließe die Datei
  }
  else {
    Serial.println("Error opening file."); 
  }
}

void loop() {
  
}
```

Wenn du den Code ausführst, solltest du den Inhalt der Textdatei auf deinem seriellen Monitor sehen:

```
Successfully opened file!
This is a test file for reading.
It contains some sample text.
```

## Tiefergehende Einblicke

Die `SD.read()` Funktion liest immer nur einen einzelnen Byte aus der Datei. Um eine vollständige Zeile auszulesen, musst du eine Schleife verwenden und jeden Buchstaben einzeln auslesen. Du kannst auch die `SD.seek()` Funktion verwenden, um an eine bestimmte Stelle in der Datei zu springen.

Es ist auch wichtig zu beachten, dass die maximale Dateigröße auf einer SD-Karte vom Dateisystem abhängt. Die meisten modernen SD-Karten unterstützen jedoch Dateigrößen bis zu 4 GB.

## Siehe auch

- SD library reference: https://www.arduino.cc/en/Reference/SD
- Reading and writing files on SD cards: https://www.arduino.cc/en/Tutorial/ReadASCIIStringFromExternalFile
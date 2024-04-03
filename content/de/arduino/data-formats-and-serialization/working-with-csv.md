---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:52.500058-07:00
description: "Wie: Arduino besitzt keine integrierte Bibliothek speziell f\xFCr den\
  \ Umgang mit CSV-Dateien, aber man kann die `SD`- und `SPI`-Bibliotheken verwenden,\
  \ um\u2026"
lastmod: '2024-03-13T22:44:54.165868-06:00'
model: gpt-4-0125-preview
summary: "Arduino besitzt keine integrierte Bibliothek speziell f\xFCr den Umgang\
  \ mit CSV-Dateien, aber man kann die `SD`- und `SPI`-Bibliotheken verwenden, um\
  \ auf Dateien auf einer SD-Karte zuzugreifen, und dann CSV-Daten mithilfe grundlegender\
  \ String-Manipulationstechniken parsen oder generieren."
title: Arbeiten mit CSV
weight: 37
---

## Wie:
Arduino besitzt keine integrierte Bibliothek speziell für den Umgang mit CSV-Dateien, aber man kann die `SD`- und `SPI`-Bibliotheken verwenden, um auf Dateien auf einer SD-Karte zuzugreifen, und dann CSV-Daten mithilfe grundlegender String-Manipulationstechniken parsen oder generieren. Bei der Verarbeitung komplexerer CSV-Manipulationen kann die Drittanbieterbibliothek `ArduinoCSV` für einfacheres Parsen und Schreiben genutzt werden.

**Lesen von CSV-Daten von einer SD-Karte:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisierung fehlgeschlagen!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Gibt die CSV-Zeile aus
    }
    dataFile.close();
  } else {
    Serial.println("Fehler beim Öffnen von data.csv");
  }
}

void loop() {
  // Wird in diesem Beispiel nicht verwendet
}
```
*Beispielausgabe:*
```
SensorID, Timestamp, Wert
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Schreiben von CSV-Daten auf eine SD-Karte:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisierung fehlgeschlagen!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Wert"); // CSV-Header
    dataFile.println("1, 1597840923, 23.5"); // Beispieldatenzeile
    dataFile.close();
    Serial.println("Daten geschrieben");
  } else {
    Serial.println("Fehler beim Öffnen von output.csv");
  }
}

void loop() {
  // Wird in diesem Beispiel nicht verwendet
}
```
*Beispielausgabe:*
```
Daten geschrieben
```

**Verwendung von ArduinoCSV zum Parsen:**
Falls man sich mit komplexen CSV-Dateien beschäftigt, kann die `ArduinoCSV`-Bibliothek das Parsen erheblich vereinfachen. Dieses Beispiel setzt voraus, dass die `ArduinoCSV`-Bibliothek bereits installiert ist.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisierung fehlgeschlagen!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Druckt jedes Feld
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Fehler beim Öffnen von data.csv");
  }
}

void loop() {
  // Wird in diesem Beispiel nicht verwendet
}
```
*Beispielausgabe:*
```
SensorID,  Timestamp,  Wert
1,  1597840923,  23.5
2,  1597840987,  22.4
```
In diesen Beispielen können Arduino-Projekte durch das Lesen von und Schreiben in CSV-Dateien auf einer SD-Karte einfach Daten sammeln, Konfigurationseinstellungen speichern oder Daten mit anderen Anwendungen in einem universell zugänglichen Format austauschen.

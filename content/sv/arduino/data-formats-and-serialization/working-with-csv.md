---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:59.016483-07:00
description: "Hur man g\xF6r: Arduino har inte ett inbyggt bibliotek specifikt f\xF6\
  r att hantera CSV-filer, men du kan anv\xE4nda `SD`- och `SPI`-biblioteken f\xF6\
  r att komma \xE5t\u2026"
lastmod: '2024-03-13T22:44:38.190411-06:00'
model: gpt-4-0125-preview
summary: "Arduino har inte ett inbyggt bibliotek specifikt f\xF6r att hantera CSV-filer,\
  \ men du kan anv\xE4nda `SD`- och `SPI`-biblioteken f\xF6r att komma \xE5t filer\
  \ p\xE5 ett SD-kort och sedan tolka eller generera CSV-data med hj\xE4lp av grundl\xE4\
  ggande tekniker f\xF6r str\xE4ngmanipulering."
title: Arbeta med CSV
weight: 37
---

## Hur man gör:
Arduino har inte ett inbyggt bibliotek specifikt för att hantera CSV-filer, men du kan använda `SD`- och `SPI`-biblioteken för att komma åt filer på ett SD-kort och sedan tolka eller generera CSV-data med hjälp av grundläggande tekniker för strängmanipulering. När du hanterar mer komplex CSV-manipulering kan det tredjepartsbiblioteket `ArduinoCSV` användas för enklare tolkning och skrivning.

**Läsa CSV-data från ett SD-kort:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initiering misslyckades!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Skriver ut CSV-rad
    }
    dataFile.close();
  } else {
    Serial.println("Fel vid öppning av data.csv");
  }
}

void loop() {
  // Används inte i detta exempel
}
```
*Exempelutskrift:*
```
SensorID, Tidsstämpel, Värde
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Skriva CSV-data till ett SD-kort:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initiering misslyckades!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Tidsstämpel, Värde"); // CSV-huvud
    dataFile.println("1, 1597840923, 23.5"); // Exempel på datarad
    dataFile.close();
    Serial.println("Data skrivet");
  } else {
    Serial.println("Fel vid öppning av output.csv");
  }
}

void loop() {
  // Används inte i detta exempel
}
```
*Exempelutskrift:*
```
Data skrivet
```

**Använda ArduinoCSV för tolkning:**
Om du hanterar komplexa CSV-filer kan `ArduinoCSV`-biblioteket avsevärt förenkla ansträngningarna för tolkning. Detta exempel antar att du redan har installerat `ArduinoCSV`-biblioteket.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initiering misslyckades!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Skriver ut varje fält
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Fel vid öppning av data.csv");
  }
}

void loop() {
  // Används inte i detta exempel
}
```
*Exempelutskrift:*
```
SensorID,  Tidsstämpel,  Värde
1,  1597840923,  23.5
2,  1597840987,  22.4
```
I dessa exempel, genom att läsa från och skriva till CSV-filer på ett SD-kort, kan Arduinoprojekt enkelt samla in data, lagra konfigurationsinställningar eller utbyta data med andra applikationer i ett universellt tillgängligt format.

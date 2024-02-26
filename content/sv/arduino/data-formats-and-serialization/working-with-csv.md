---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:59.016483-07:00
description: "Att arbeta med CSV-filer (kommaseparerade v\xE4rden) i Arduino inneb\xE4\
  r att l\xE4sa fr\xE5n och skriva till CSV-filer som vanligtvis lagras p\xE5 ett\
  \ SD-kort, vilket\u2026"
lastmod: '2024-02-25T18:49:36.505226-07:00'
model: gpt-4-0125-preview
summary: "Att arbeta med CSV-filer (kommaseparerade v\xE4rden) i Arduino inneb\xE4\
  r att l\xE4sa fr\xE5n och skriva till CSV-filer som vanligtvis lagras p\xE5 ett\
  \ SD-kort, vilket\u2026"
title: Arbeta med CSV
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV-filer (kommaseparerade värden) i Arduino innebär att läsa från och skriva till CSV-filer som vanligtvis lagras på ett SD-kort, vilket möjliggör datalogging, konfigurationsinställningar och mer. Programmerare hanterar ofta CSV-filer för insamling av sensordata, lagring av konfigurationsparametrar eller gränssnitt med andra system på grund av dess enkelhet och breda acceptans över plattformar.

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

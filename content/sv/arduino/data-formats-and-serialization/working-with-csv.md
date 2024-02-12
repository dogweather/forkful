---
title:                "Arbeta med CSV"
date:                  2024-02-03T19:18:59.016483-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

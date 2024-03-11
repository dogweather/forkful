---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:11.315551-07:00
description: "\xC5 arbeide med CSV-filer (Comma-Separated Values) i Arduino involverer\
  \ lesing fra og skriving til CSV-filer som vanligvis lagres p\xE5 et SD-kort, noe\
  \ som\u2026"
lastmod: '2024-03-11T00:14:14.673027-06:00'
model: gpt-4-0125-preview
summary: "\xC5 arbeide med CSV-filer (Comma-Separated Values) i Arduino involverer\
  \ lesing fra og skriving til CSV-filer som vanligvis lagres p\xE5 et SD-kort, noe\
  \ som\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å arbeide med CSV-filer (Comma-Separated Values) i Arduino involverer lesing fra og skriving til CSV-filer som vanligvis lagres på et SD-kort, noe som muliggjør datalogging, konfigurasjonsinnstillinger og mer. Programmerere håndterer ofte CSV-filer for datainnsamling fra sensorer, lagring av konfigurasjonsparametere eller grensesnitt med andre systemer, på grunn av dets enkelhet og brede adopsjon på tvers av plattformer.

## Hvordan:
Arduino har ikke et innebygget bibliotek spesifikt for håndtering av CSV-filer, men du kan bruke `SD` og `SPI` bibliotekene for å få tilgang til filer på et SD-kort, og deretter parse eller generere CSV-data ved hjelp av grunnleggende strengmanipuleringsteknikker. Når du håndterer mer kompleks CSV-manipulering, kan det tredjeparts biblioteket `ArduinoCSV` benyttes for enklere parsing og skriving.

**Lese CSV-data fra et SD-kort:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisering mislyktes!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Skriver ut CSV-linjen
    }
    dataFile.close();
  } else {
    Serial.println("Feil ved åpning av data.csv");
  }
}

void loop() {
  // Ikke brukt i dette eksempelet
}
```
*Eksempel på utdata:*
```
SensorID, Tidsstempel, Verdi
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Skrive CSV-data til et SD-kort:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisering mislyktes!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Tidsstempel, Verdi"); // CSV-header
    dataFile.println("1, 1597840923, 23.5"); // Eksempel på datarad
    dataFile.close();
    Serial.println("Data skrevet");
  } else {
    Serial.println("Feil ved åpning av output.csv");
  }
}

void loop() {
  // Ikke brukt i dette eksempelet
}
```
*Eksempel på utdata:*
```
Data skrevet
```

**Bruke ArduinoCSV for Parsing:**
Hvis du håndterer komplekse CSV-filer, kan `ArduinoCSV` biblioteket i stor grad forenkle parsingarbeidet. Dette eksempelet forutsetter at du allerede har installert `ArduinoCSV` biblioteket.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisering mislyktes!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Skriver ut hvert felt
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Feil ved åpning av data.csv");
  }
}

void loop() {
  // Ikke brukt i dette eksempelet
}
```
*Eksempel på utdata:*
```
SensorID,  Tidsstempel,  Verdi
1,  1597840923,  23.5
2,  1597840987,  22.4
```
I disse eksemplene, ved å lese fra og skrive til CSV-filer på et SD-kort, kan Arduino-prosjekter enkelt samle inn data, lagre konfigurasjonsinnstillinger, eller utveksle data med andre applikasjoner i et universelt tilgjengelig format.

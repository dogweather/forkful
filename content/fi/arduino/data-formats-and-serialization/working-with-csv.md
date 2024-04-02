---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:08.375240-07:00
description: "CSV-tiedostojen (pilkulla erotetut arvot) k\xE4sittely Arduinossa k\xE4\
  sitt\xE4\xE4 lukemisen ja kirjoittamisen CSV-tiedostoihin, joita yleisesti s\xE4\
  ilytet\xE4\xE4n SD-\u2026"
lastmod: '2024-03-13T22:44:56.845669-06:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkulla erotetut arvot) k\xE4sittely Arduinossa k\xE4\
  sitt\xE4\xE4 lukemisen ja kirjoittamisen CSV-tiedostoihin, joita yleisesti s\xE4\
  ilytet\xE4\xE4n SD-\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Mikä & Miksi?
CSV-tiedostojen (pilkulla erotetut arvot) käsittely Arduinossa käsittää lukemisen ja kirjoittamisen CSV-tiedostoihin, joita yleisesti säilytetään SD-kortilla. Tämä mahdollistaa datan loggauksen, konfiguraatioasetukset ja paljon muuta. Ohjelmoijat käsittelevät usein CSV-tiedostoja sensoridatan keräämiseen, konfiguraatioparametrien tallennukseen tai muiden järjestelmien kanssa kommunikoimiseen niiden yksinkertaisuuden ja laajan yhteensopivuuden vuoksi eri alustoilla.

## Kuinka:
Arduinossa ei ole sisäänrakennettua kirjastoa erityisesti CSV-tiedostojen käsittelyyn, mutta voit käyttää `SD` ja `SPI` kirjastoja tiedostojen käyttämiseen SD-kortilla, ja sitten jäsentää tai luoda CSV-dataa käyttäen perus merkkijonokäsittelyn tekniikoita. Kun käsitellään monimutkaisempia CSV-tiedostojen manipulointeja, kolmannen osapuolen kirjastoa `ArduinoCSV` voidaan hyödyntää helpottamaan jäsentämistä ja kirjoittamista.

**CSV-datan lukeminen SD-kortilta:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Alustus epäonnistui!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Tulostaa CSV-rivin
    }
    dataFile.close();
  } else {
    Serial.println("Virhe avattaessa data.csv");
  }
}

void loop() {
  // Ei käytetä tässä esimerkissä
}
```
*Esimerkkituloste:*
```
SensorID, Aikaleima, Arvo
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**CSV-datan kirjoittaminen SD-kortille:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Alustus epäonnistui!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Aikaleima, Arvo"); // CSV otsikko
    dataFile.println("1, 1597840923, 23.5"); // Esimerkki datarivistä
    dataFile.close();
    Serial.println("Data kirjoitettu");
  } else {
    Serial.println("Virhe avattaessa output.csv");
  }
}

void loop() {
  // Ei käytetä tässä esimerkissä
}
```
*Esimerkkituloste:*
```
Data kirjoitettu
```

**ArduinoCSV:n käyttö jäsentämiseen:**
Jos käsittelet monimutkaisia CSV-tiedostoja, `ArduinoCSV` kirjasto voi huomattavasti yksinkertaistaa jäsentämisprosessia. Tämä esimerkki olettaa, että olet jo asentanut `ArduinoCSV` kirjaston.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Alustus epäonnistui!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Tulostaa jokaisen kentän
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Virhe avattaessa data.csv");
  }
}

void loop() {
  // Ei käytetä tässä esimerkissä
}
```
*Esimerkkituloste:*
```
SensorID, Aikaleima, Arvo
1, 1597840923, 23.5
2, 1597840987, 22.4
```
Näiden esimerkkien avulla, lukemalla ja kirjoittamalla CSV-tiedostoja SD-kortille, Arduino-projektit voivat helposti kerätä dataa, tallentaa konfiguraatioasetuksia tai vaihtaa tietoja muiden sovellusten kanssa yleisesti saatavilla olevassa muodossa.

---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"
Tekstitiedoston kirjoittaminen tarkoittaa tiedon tallentamista tekstimuodossa laitteelle. Ohjelmoijat tekevät sen datan säilyttämiseen, lokien kirjaamiseen tai asetusten tallentamiseen.

## How to: 
"Kuinka:"
```Arduino
#include <SD.h>

File tiedosto;

void setup() {
  // Käynnistä sarjaliikenne
  Serial.begin(9600);
  // Oleta, että SD-kortti on liitettynä pin 4
  if (!SD.begin(4)) {
    Serial.println("SD-kortin alustus epäonnistui!");
    return;
  }
  // Luo ja avaa teksti.txt kirjoitusta varten
  tiedosto = SD.open("teksti.txt", FILE_WRITE);
  
  // Tarkista, voitiinko tiedosto avata
  if (tiedosto) {
    tiedosto.println("Hei Arduino!");
    tiedosto.close(); // Sulje tiedosto
    Serial.println("Kirjoitus onnistui.");
  } else {
    Serial.println("Tiedoston avaaminen epäonnistui.");
  }
}

void loop() {
  // Tyhjä silmukka
}
```

## Deep Dive
"Sukellus syvyyksiin"
Historiallisesti teksti- ja datatiedostot ovat olleet tärkeitä koneiden ja ohjelmien välisessä kommunikoinnissa. SD-kortille kirjoittamisen vaihtoehtoja ovat EEPROM tai etäpalvelimelle lähetys. Arduino käyttää FAT16/32-tiedostojärjestelmiä ja kirjoittamisen toteutus riippuu käytettävästä laitteistosta ja kirjastosta.

## See Also
"Katso myös"
- Arduino SD-kirjaston opas: https://www.arduino.cc/en/Reference/SD
- EEPROMin kirjoittaminen Arduinolla: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMWrite
- FAT16/32-tiedostojärjestelmä: https://www.självklart.fi/fat16-32

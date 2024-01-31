---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "CSV-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja miksi?
CSV (Comma-Separated Values) on tiedostomuoto tietojen tallentamiseen helppolukuiseen muotoon. Ohjelmoijat käyttävät sitä yksinkertaisten tietojen vaihtoon ja säilytykseen, koska se on yhteensopiva monien ohjelmistojen kanssa.

## How to: - Kuinka tehdä:
```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  
  // Aloita SD-kortti moodissa 4
  if (!SD.begin(4)) {
    Serial.println("SD card failed or not present");
    return;
  }
  // Lue CSV-tiedosto
  myFile = SD.open("data.csv");
  if (myFile) {
    while (myFile.available()) {
      String data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    myFile.close();
  } else {
    Serial.println("error opening data.csv");
  }
}

void loop() {
  // Ei loop-koodia tässä esimerkissä
}
```
**Tuloste:**
```
sensor1, value1
sensor2, value2
sensor3, value3
...
```

## Deep Dive - Syväsukellus
CSV-muoto on aikojen saatossa yleistynyt, koska sen yksinkertaisuus tekee tiedonsiirrosta helppoa. Vaihtoehtoiset formaatit, kuten JSON tai XML, tarjoavat enemmän ominaisuuksia mutta ovat monimutkaisempia. Arduinoissa CSV:n käsittely rajoittuu perus tiedostojen käsittelyyn, koska laitteissa ei yleensä ole paljon muistia tai suoritustehoa.

## See Also - Katso myös:
- Arduino SD-kirjaston dokumentaatio: https://www.arduino.cc/en/Reference/SD
- CSV:n tarkempi kuvaus ja erityiskäyttökohteet: https://tools.ietf.org/html/rfc4180
- JSONin ja XML:n vertailu CSV:hen: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
- Esimerkkejä ja oppaita CSV:n käsittelystä eri ohjelmointikielillä: https://www.w3schools.com/python/python_file_handling.asp

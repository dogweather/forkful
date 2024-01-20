---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Luettaessa tekstitiedostoa otetaan tiedostosta ymmärrettävää dataa koodikäyttöön. Se on tärkeää, koska sen avulla ohjelmoijat voivat tallentaa tai käyttää suuria datamääriä ohjelmissaan.

## Näin se tehdään:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    while (1);
  }
  myFile = SD.open("test.txt");
  if (myFile) {
    Serial.println("test.txt:");
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("error opening test.txt");
  }
}

void loop() {
  // nothing happens after setup
}
```

Tässä koodissa luemme tekstitiedostoa nimeltä `test.txt` SD-kortilta ja tulostamme sen sisällön sarjamonitoriin.

## Syvällisempi sukellus:

Historiallisesti tekstitiedostojen lukeminen on ollut yksi ensimmäisistä tavoista tallentaa ja hakea dataa. Arduino-yhteisössä käyttäjät ovat kehittäneet useita kirjastoja, jotka helpottavat tiedostojen käsittelyä, kuten edellä mainittu SD-kirjasto.

Yksi vaihtoehtoinen menetelmä tiedostojen lukemiseen on EEPROM, joka on pysyvä tallennustila, jonka avulla voit tallentaa ja lukea dataa, vaikka laite sammutettaisiin.

Tekstitiedoston lukeminen Arduinolla edellyttää SD-kirjaston käyttöönottoa, joka hallitsee tiedostojen avaamisen, lukemisen ja sulkeutumisen.

## Katso myös:

- [SD-kirjaston dokumentaatio](https://www.arduino.cc/en/reference/SD)
- [EEPROM-kirjaston dokumentaatio](https://www.arduino.cc/en/Reference/EEPROM)
- [Tutorial tekstitiedoston luomisesta ja kirjoittamisesta](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
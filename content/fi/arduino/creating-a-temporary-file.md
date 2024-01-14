---
title:                "Arduino: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Miksi luoda väliaikainen tiedosto Arduino-ohjelmoinnissa?

Jos olet koskaan työskennellyt Arduino-ohjelmoinnin parissa, tiedät miten tärkeää on tallentaa ja käsitellä tietoa tarkasti. Joissakin tapauksissa saattaa olla tarpeen luoda väliaikainen tiedosto, johon tallennetaan tilapäistä tietoa tai väliaikainen versio tiedostosta, johon haluat tehdä muutoksia. Tässä blogikirjoituksessa näytämme, miten luodaan väliaikainen tiedosto Arduino-ohjelmoinnissa ja miten sitä käytetään.

Kuinka luoda väliaikainen tiedosto Arduino-ohjelmoinnissa?

Voit luoda väliaikaisen tiedoston Arduino-ohjelmoinnissa ```tempFile```-muuttujan avulla. Tämä muuttuja luodaan käyttämällä ```File```-kirjastoa, jota voidaan käyttää tiedostojen luomiseen, avaamiseen ja muuhun käsittelyyn.

```
#include <SPI.h>
#include <SD.h>

const int chipSelect = 10; // SD-kortin liitäntä

File tempFile; // Luo "tempFile"-muuttuja

void setup() {
  // Alustetaan SD-moduuli
  SD.begin(chipSelect);
  
  // Luodaan tiedosto "temp.txt"
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  // Kirjoitetaan tiedostonumeron arvo
  tempFile.println(1234);
  
  // Suljetaan tiedosto
  tempFile.close();
}

void loop() {
  // Tässä voit tehdä muutoksia tiedostoon ja tallentaa ne
  // Väliaikainen tiedosto säilyy, kunnes moduuli sammutetaan
  
  // Lopuksi voit poistaa tiedoston, kun sitä ei enää tarvita
  SD.remove("temp.txt");
}
```

Deep Dive: Väliaikaisen tiedoston luominen

Väliaikainen tiedosto luodaan käyttämällä ```FILE_WRITE```-tilaa, joka tarkoittaa, että tiedostoon voi kirjoittaa tietoa. Voit myös käyttää ```FILE_READ```-tilaa, jos haluat lukea tiedostoa. Tiedosto poistetaan lopuksi käyttämällä ```SD.remove()```-toimintoa. Väliaikainen tiedosto on hyödyllinen silloin, kun tarvitset tallentaa tilapäistä tietoa, jota ei tarvita pitkäaikaiseen tallennukseen.

## Katso myös

- [Arduino-ohjelmointiopas](https://www.arduino.cc/en/Guide/Introduction)
- [SD-kortin käyttö Arduino-ohjelmoinnissa](https://www.arduino.cc/en/Reference/SD)
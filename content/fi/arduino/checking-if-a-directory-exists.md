---
title:                "Tarkistetaan, onko hakemisto olemassa"
date:                  2024-01-19
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Tarkistetaan, onko hakemisto olemassa, jotta tiedostot eivät katoa tyhjyyteen. Koodarit tekevät tämän, koska se säästää aikaa ja estää virheitä.

## How to: - Miten:
```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("SD-kortin alustus epäonnistui");
    return;
  }
  
  File root = SD.open("/");
  if (root.isDirectory()) {
    Serial.println("Pääkansio on olemassa");
  } else {
    Serial.println("Pääkansiota ei löydy");
  }  
}

void loop() {
  // Ei koodia loopissa tässä esimerkissä
}
```
Tuloste:
```
Pääkansio on olemassa
```

## Deep Dive - Sukellus syvemmälle
Hakemiston olemassaolon tarkistus on olennainen toiminto, kun käsitellään tiedostoja mikrokontrollerissa. Historiallisesti, tietokoneissa, tämä on tapahtunut käyttöjärjestelmän tarjoamien työkalujen kautta, mutta Arduinossa käytämme `SD`-kirjastoa, joka interfaceaa SD-kortille SPI-väylän kautta. Vaihtoehtoisesti, voit käyttää `SdFat`-kirjastoa, joka on tehokkaampi mutta monimutkaisempi. Tarkistuksessa `isDirectory()`-funktio tarkastaa, onko tiedosto, jonka avasit, hakemisto. Mitään ei tapahdu, jos hakemistoa ei ole; siksi on hyvä tarkistaa ensin.

## See Also - Katso myös
- Arduinon SD-kirjaston dokumentaatio: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- `SdFat`-kirjaston GitHub-sivu: [https://github.com/greiman/SdFat](https://github.com/greiman/SdFat)
- SPI-väylän yleiskatsaus: [https://www.arduino.cc/en/reference/SPI](https://www.arduino.cc/en/reference/SPI)

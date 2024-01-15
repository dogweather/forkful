---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Arduino: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä törmännyt tilanteeseen, jossa haluat tarkistaa, onko tietyssä hakemistossa olemassa olevia tiedostoja tai kansioita. Tämä on hyödyllistä ennen esimerkiksi tiedoston avaamista ja käsittelyä, jotta vältetään virheet ja ohjelma toimii sujuvasti. Jatkossa kerromme, miten tätä tarkistusta voidaan toteuttaa Arduino-ohjelmoinnissa.

## Miten tehdä

```Arduino
#include <SD.h>

void setup() {
  // Alustetaan SD-kortin tiedostonhallinta
  if (!SD.begin(10)) {
    // Tulostetaan viesti, jos SD-korttia ei löydetty
    Serial.println("SD-korttia ei löydy");
    while (1);
  }

  // Tarkistetaan, onko hakemisto olemassa
  if (!SD.exists("/hakemisto/")) {
    // Tulostetaan viesti, jos hakemistoa ei löydy
    Serial.println("Hakemistoa ei löydy");
    // Tehdään halutut toimenpiteet, jos hakemistoa ei löytynyt
  } else {
    // Tehdään halutut toimenpiteet, jos hakemisto löytyi
  }
}

void loop() {

}
```

Tässä esimerkissä käytämme SD-kortin tiedostonhallinta-kirjastoa (SD.h). Aluksi alustamme SD-kortin `SD.begin()` ja tarkistamme, onko korttia löydetty `!SD.begin()`. Jos korttia ei löydy, tulostetaan kyseinen viesti ja ohjelma pysähtyy `while`-silmukkaan. Seuraavaksi tarkistamme, onko hakemisto olemassa `SD.exists("/hakemisto/")`. Jos hakemistoa ei löydy, tulostetaan viesti ja voidaan tehdä halutut toimenpiteet, kuten luoda uusi hakemisto `SD.mkdir()` tai ladata tiedosto `SD.open()` ja tallentaa se hakemistoon.

## Syvempi sukellus

Tarkasteltaessa `SD.exists()`-funktiota tarkemmin, huomaamme sen palauttavan `true` tai `false`-arvon riippuen siitä, löytyykö hakemisto vai ei. Funktio tarkistaa, onko tiedoston koko nolla tavua vai ei. Jos tiedosto on tyhjä, funktio palauttaa `false`. Muissa tapauksissa se palauttaa `true`, vaikka hakemistossa ei olisi tiedostoja tai alihakemistoja.

Tämä tarkistus on hyödyllinen myös muissa tilanteissa, kuten tiedostojen kopiointiin tai kansioiden läpikäyntiin.

## Katso myös

- SD-Kirjastodokumentaatio: https://www.arduino.cc/en/reference/SD
- SD-kirjaston esimerkit: https://www.arduino.cc/en/Tutorial/LibraryExamples/SD
- SD-tiedostojärjestelmä: https://www.arduino.cc/en/Reference/FileExtensionSD
---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin tiedostoon kirjoittaminen tarkoittaa tietojen tallentamista tekstimuodossa olevaan tiedostoon. Tätä tekevät ohjelmoijat, jotta he voivat tallentaa ja käsitellä tietoa järjestelmällisesti.

## Kuinka:

Koodiesimerkit ja näytöslähdemateriaali sisällä ```Arduino ... ``` koodilohkoissa.

```arduino
// Luo tiedosto nimeltään "data.txt" ja avaa se kirjoittamista varten
File tiedosto = SD.open("data.txt", FILE_WRITE);

// Tarkista, onko tiedostoa onnistuneesti luotu ja avattu
if (tiedosto) {
  // Tallenna tiedostoon "Hello world!"
  tiedosto.println("Hello world!");
  // Sulje tiedosto
  tiedosto.close();
  // Tulosta ilmoitus onnistumisesta
  Serial.println("Tiedosto tallennettu!");
} else {
  // Tulosta virheilmoitus, jos tiedoston luominen ja avaaminen epäonnistui
  Serial.println("Virhe luotaessa ja avaamalla tiedostoa.");
}
```

## Syväsukellus:

Historiallinen konteksti: Tekstin tiedostoon kirjoittaminen oli alun perin yksi tapa tallentaa ja käsitellä tietoa tietokoneissa, mutta nykyään se on yhä yleisempi tapa tallentaa tietoa myös sulautetuissa järjestelmissä, kuten Arduinoissa.

Vaihtoehtoiset menetelmät: Tietojen tallentaminen tekstimuodossa olevaan tiedostoon ei ole ainoa tapa tallentaa ja käsitellä tietoa. Esimerkiksi tietokantojen käyttö ja yhteydenpito pilvipalveluihin ovat myös yleisiä tapoja tallentaa ja käsitellä dataa.

Toteutuksen yksityiskohdat: Tekstin tiedostoon kirjoittaminen Arduinoissa vaatii muistikortin käyttöä ja sen oikean formaatin käyttöönottoa. Muistikortin tulee olla FAT16- tai FAT32-formaatissa, jotta Arduino pystyy lukemaan ja kirjoittamaan tiedostoon.

## Katso myös:

Hyödyllisiä linkkejä tekstien tiedostoon kirjoittamiseen liittyen:

- [Tietojen tallentaminen Arduinoissa](https://playground.arduino.cc/Main/InterfacingWithHardware#filesave)
- [Fat16 - kirjasto SD-kortille](https://github.com/greiman/SdFat)
- [Esimerkkejä tekstien tiedostoon kirjoittamisesta](https://www.arduino.cc/en/Tutorial/Files)
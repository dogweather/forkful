---
title:    "Arduino: Tiedostotyyppien lukeminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Arduino-ohjelmointi on hauska ja koukuttava harrastus, joka antaa käyttäjille mahdollisuuden toteuttaa omia ideoitaan ja projektejaan. Yksi tärkeä osa Arduino-ohjelmointia on tiedostojen lukeminen, joka antaa mahdollisuuden käsitellä ja tallentaa dataa helposti. Tämä blogipostaus tarjoaa opastusta tiedostojen lukemiseen Arduino-ohjelmoinnissa suomenkielisille lukijoille.

## Miten

Kun käytetään Arduinoa tiedostojen lukemiseen, tulee ensin avata haluttu tiedosto. Tämän voi tehdä käyttämällä `SD.open()` komentoa, joka ottaa parametrinä tiedoston nimen.

```Arduino
File tiedosto = SD.open("tiedostonimi.txt");

```

Tämän jälkeen tiedosto voidaan lukea rivi kerrallaan `tiedosto.readln()` komennolla ja tallentaa muuttujaan. Esimerkiksi seuraavassa koodissa luetaan tiedosto ja tulostetaan sen sisältö sarjaliikenteen avulla:

```Arduino
File tiedosto = SD.open("tiedostonimi.txt");
if (tiedosto) {
  while (tiedosto.available()) {
    Serial.println(tiedosto.readln());
  }
  tiedosto.close();
}
```

Kun tiedosto on luettu loppuun, se tulee sulkea komennolla `tiedosto.close()`, jotta resursseja ei jää suorituksen ajaksi käyttämättä.

## Syväsukellus

Arduino tukee myös tiettyjä tiedostojen manipulointiin liittyviä komentoja, esimerkiksi `tiedosto.seek()` komennolla voi siirtyä haluttuun kohtaan tiedostossa. Tiedostojen lukeminen ja kirjoittaminen on myös mahdollista käyttäen `SPI` kirjastoa, mikä mahdollistaa nopeamman tiedonsiirron.

Käyttäjän tulee aina muistaa tarkistaa, että tiedosto on avattu onnistuneesti, ennen kuin yritetään lukea tai kirjoittaa siihen. Tiedostojen käsittelyyn liittyy myös muistinhallintaan liittyviä haasteita, joten on tärkeää avata ja sulkea tiedosto aina tarvittaessa.

## Katso myös

- [Arduino tiedostojen käsittely](https://www.arduino.cc/en/Reference/SD)
- [SPI kirjasto Arduino](https://www.arduino.cc/en/Reference/SPI)
- [Arduino tiedostojärjestelmät](https://www.arduino.cc/en/Reference/FSTruncate)
- [Arduino tiedoston avaaminen uudestaan](https://www.arduino.cc/en/Reference/Remove)







####### Katso myös
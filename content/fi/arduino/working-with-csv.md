---
title:                "Työskentely csv:n kanssa"
html_title:           "Arduino: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi: CSV:n käsittelyn tärkeys

CSV-tiedostojen käsittely on yleinen tehtävä, kun työskentelet tietojen tallentamisen ja jakamisen kanssa. Tiedostomuoto on helppokäyttöinen ja yhteensopiva monien ohjelmistojen kanssa. CSV-tiedostot voivat sisältää suuria määriä tietoja, jotka voidaan jäsennellä ja lukea helposti. Arduino voi helposti käsitellä ja käyttää näitä tiedostoja, joten tutustutaan lisää CSV:n käsittelyyn.

## Kuinka: Esimerkkejä koodista ja tulostuksesta

Arduino-mikrokontrollerilla on kyky lukea ja käsitellä CSV-tiedostoja. Käytämme esimerkkinä yksinkertaista tiedostoa, joka sisältää lämpötila- ja kosteustietoja.

```Arduino
#include <SD.h>
File tiedosto; //taulukon esilataus tiedostolle

void setup(){
  Serial.begin(9600); //alustetaan sarjaporttiliitäntä
  SD.begin(10); //alustetaan tiedostokirjasto ja määritetään CS-pin
  tiedosto = SD.open("data.csv"); //avaa tiedosto nimeltä data.csv
}

void loop(){
  if (tiedosto.available()){ //jos tiedosto on saatavilla
    while (tiedosto.available()){ //käydään läpi koko tiedosto
      Serial.write(tiedosto.read()); //lähetetään tiedosto sarjaporttiin
    }
    tiedosto.close(); //suljetaan tiedosto
  }
}
```

Tämän esimerkkikoodin avulla voidaan lukea ja tulostaa kaikki CSV-tiedoston tiedot sarjaporttiin. Tämä helpottaa tietojen tarkastelua ja mahdollistaa niiden käytön esimerkiksi graafisen käyttöliittymän luomisessa.

## Syväsukellus: Tietoa CSV:n käsittelystä

CSV-tiedostot koostuvat taulukoista, joissa tietoja on jaettu pilkulla tai muulla erottajalla. Näitä tiedostoja käytetään yleisesti tallentamaan suuria määriä tietoja, kuten mittauksia tai tiedostoja, jotka sisältävät erilaisia numero- tai tekstiarvoja. Arduino-koodilla voidaan helposti käsitellä kaikenlaisia tiedostoja ja jopa luoda uusia tiedostoja tarpeen mukaan.

## Katso myös

- [Arduino-sovellus CSV-tiedostojen käsittelyyn] (https://www.arduino.cc/en/Reference/SD)
- [CSV-tiedostojen lukeminen ja kirjoittaminen Arduinolla] (https://www.arduino.cc/en/Tutorial/SimpleReadWrite)
- [SD-kirjaston tiedot ja funktiot] (https://www.arduino.cc/en/Reference/SD)
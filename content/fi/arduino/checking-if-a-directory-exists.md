---
title:    "Arduino: Tarkistetaan, onko hakemisto olemassa"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Tuntuuko joskus siltä, että rakentaessasi Arduino-projektia, joudut jatkuvasti tarkistamaan, onko tietty hakemisto olemassa? Kun tiedät, että hakemisto on olemassa, voit suunnitella koodisi sen mukaan. Tämä auttaa sinua estämään mahdollisia ongelmia ja vähentämään virhekoodin määrää.

## Miten

```Arduino
#include <SPI.h>
#include <SD.h>

File directory;

void setup(){
  Serial.begin(9600);

  // Liitetään SD-kortti
  if(!SD.begin(10)){
    Serial.println("Error");
  }

  // Tarkistetaan hakemisto
  if(SD.exists("/hakemisto")){
    // Jos hakemisto on olemassa, luodaan uusi tiedosto hakemistoon
    directory = SD.open("/hakemisto/uusi_tiedosto.txt", FILE_WRITE);
    directory.println("Tämä on uusi tiedosto!");
    directory.close();
  }
  else{
    Serial.println("Hakemistoa ei löytynyt.");
  }
}

void loop(){

}
```

### Selitys

Yllä olevassa esimerkkikoodissa olemme liittäneet SD-kortin (10-pin porttiin) ja avanneet tiedostojärjestelmän. Ensimmäisessä if-lausekkeessa tarkistamme, onko hakemistoa nimeltä "hakemisto" olemassa. Jos se on, avataan uusi tiedosto nimeltä "uusi_tiedosto.txt" hakemistoon FILE_WRITE -tilassa ja kirjoitetaan siihen tekstirivi. Lopuksi tiedosto suljetaan.

Jos hakemistoa ei löydy, tulostetaan virheilmoitus sarjaväylään.

## Syväsukellus

Hakemiston tarkistamisella on myös muita käyttötarkoituksia. Voit esimerkiksi yksinkertaisesti tarkistaa, onko tiedosto olemassa ennen sen avaamista tai jopa poistamista. Voit myös käyttää tätä toimintoa luodaksesi uusia hakemistoja tai saadaksesi tietoa hakemiston sisällöstä.

## Katso myös

- [SD-kortti kirjanpitäjänä](https://www.arduino.cc/en/Reference/SDbegin)
- [SD-hakemiston tarkistaminen](https://www.arduino.cc/en/Reference/SDexists)
- [SD-kortin kirjoittaminen](https://www.arduino.cc/en/Reference/SDopen)
---
title:                "Tarkastetaan, onko kansio olemassa"
html_title:           "Arduino: Tarkastetaan, onko kansio olemassa"
simple_title:         "Tarkastetaan, onko kansio olemassa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tiedoston tarkistaminen, onko kansio olemassa, on koodin käyttöä jolla tarkistetaan, onko annetun polun kansio olemassa. Tätä tehdään yleensä ohjelmassa ennen kansioon tallentamista tai lukemista, jotta vältyttäisiin virheiltä.

## Kuinka:
Alla esimerkki Arduino-koodista, jolla voidaan tarkistaa mikäli kansio "myFolder" on olemassa. Koodi tulostaa serial monitoriin "Kansio on olemassa" tai "Kansiota ei löytynyt", riippuen olemassaolosta.

```
Arduino void setup() {
  Serial.begin(9600); //Asetetaan serial monitorin nopeus
  if (SD.exists("myFolder")) { //Tarkistetaan onko kansio olemassa
    Serial.println("Kansio on olemassa");
  } else {
    Serial.println("Kansiota ei löytynyt");
  }
}
```

## Syväsukellus:
Tiedoston tarkistaminen on tärkeä osa koodin suorittamista, jotta vältyttäisiin esimerkiksi tallennus- tai lukuvirheiltä. Tämä on osa koodia, jolla varmistetaan, että ohjelma toimii oikein ja virheiltä vältytään.

Erilaisia tapoja tarkistaa tiedoston olemassaolo on olemassa, joista yksi on SD.exists()-funktio, joka tarkistaa onko kansio olemassa SD-kortilla. Toinen vaihtoehto on käyttää File-kirjaston open()-funktiota, joka yrittää avata kansioon liittyvää tiedostoa. Jos tiedostoa ei löydy, kansio on todennäköisesti olemassa.

## Katso myös:
- SD-kortin käyttö Arduino-ympäristössä: https://www.arduino.cc/en/Reference/SD
- File-kirjasto: https://www.arduino.cc/en/Reference/File
- SD.exists()-funktio: https://www.arduino.cc/en/Reference/SDexists
- File.open()-funktio: https://www.arduino.cc/en/Reference/FileOpen
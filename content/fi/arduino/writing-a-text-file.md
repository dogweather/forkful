---
title:    "Arduino: Tiedoston kirjoittaminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on tärkeä osa kehittyvän teknologian oppimiskokemusta, ja Arduino-koodin kirjoittaminen on yksi tapa harjoittaa taitoa. Tekstin kirjoittaminen Arduino-projekteissa voi auttaa sinua dokumentoimaan koodiasi, vianetsintää ja jopa tallentaa tietoja projektesi edistymisestä tulevaisuudessa.

## Miten tehdä sitä

Kirjoittamisen aloittaminen Arduino:ssa on helppoa! Käytä vain ```Arduino Serial.print(); ``` -funktiota haluamasi tiedon tulostamiseen sarjaporttiin. Esimerkiksi, jos haluat tulostaa tekstin "Tämä on tekstiä" sarjaporttiin, kirjoita ```Arduino Serial.print("Tämä on tekstiä"); ```

Tämän jälkeen suorita seuraavat vaiheet:

1. Avaa Arduino IDE.
2. Klikkaa "File" ja sitten "New".
3. Kirjoita haluamasi tiedot ```Arduino Serial.print(); ``` -funktioon.
4. Yhdistä Arduino Unoon tai muuhun yhteensopivaan laitteeseen.
5. Klikkaa "Tools" ja sitten "Serial Monitor" avataksesi sarjaportin.
6. Voit nyt nähdä tulostuksen Arduino Serial Monitorissa.

Toivottavasti tämä auttaa sinua aloittamaan tiedostojen kirjoittamisen Arduino-projektiisi!

## Syvempi sukellus

Halutessasi voit tallentaa tulostamasi tiedot tekstitiedostoon sarjaportin sijaan. Tämä voidaan tehdä käyttämällä SD-kortteja tai EEPROM-muistia. Näiden avulla voit tallentaa ja lukea minkä tahansa tyyppisiä tietoja, kuten numeerisia arvoja ja merkkijonoja.

Voit myös tutustua muihin tapoihin tallentaa tietoja, kuten käyttämällä TFT-näyttöä tai WiFi-yhteyttä.

## Katso myös

- https://www.arduino.cc/en/Reference/SerialPrint
- https://www.arduino.cc/en/Tutorial/Series
- https://www.arduino.cc/en/Reference/FileWrite
- https://www.arduino.cc/en/Reference/EEPROM
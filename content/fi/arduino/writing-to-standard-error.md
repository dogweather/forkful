---
title:                "Arduino: Tiedon kirjoittaminen standardivirheeseen"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi: Miksi kirjoittaa standardi virheeseen?

On monia syitä miksi kirjoittaa tiedot standardi virheeseen (standard error). Yksi yleisimmistä syistä on virheiden käsittely. Kun ohjelmassa tapahtuu virhe, se kirjoitetaan standardi virheeseen, mikä helpottaa sen löytämistä ja korjaamista.

## Miten: Esimerkkejä ja koodinpätkiä kirjoittamisesta standardi virheeseen

#### ```Arduinovoid setup() {
  Serial.begin(9600);
  Serial.println("Ohjelma aloitettu.");
}

void loop() {
  int luku = analogRead(A0); //luetaan tulon arvo
  if (luku < 500) { //jos arvo alle 500
    //kirjoitetaan virhe standardi virheeseen
    Serial.print("Tulon arvo on liian pieni: ");
    Serial.println(luku);
  }
}
```
Esimerkissä luetaan analogisen tulon arvo ja jos se on alle 500, kirjoitetaan virhe standardi virheeseen. Tämä esimerkki osoittaa, miten könnykkästi virhekirjoitukset voidaan toteuttaa Arduino-koodissa.

## Syvemmälle: Tietoa kirjoittamisesta standardi virheeseen

Kirjoittaminen standardi virheeseen on tärkeää virheiden käsittelyn lisäksi myös siksi, että se erottaa virheet normaalista tulostuksesta. Kun tulostus ja virheet ovat erillään, helpottaa se koodin lukemista ja ylläpitoa. Myös monimutkaisemmissa ohjelmissa, joissa on useita tiedostoja, standardi virheeseen kirjoittaminen auttaa löytämään virheet nopeammin.

## Katso myös

- [Arduino Serial.begin()](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
- [Arduino Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino Debugging](https://www.arduino.cc/en/Tutorial/debugging)
- [C++ Standard Error Output](http://www.cplusplus.com/reference/cstdio/stderr/)
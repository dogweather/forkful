---
title:                "Arduino: Tiedostotiedoston kirjoittaminen"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Bloggaaminen on suosittu tapa jakaa tietoa ja mielipiteitä. Tekstitiedostoja voidaan käyttää monella tavalla, kuten tallentamaan tärkeitä tietoja, luomaan tekstimuotoisia ohjeita tai tallentamaan muistiinpanoja. Arduino-ohjelmoinnin avulla voit myös käyttää tekstitiedostoja osana projektejasi.

## Kuinka

Aloittaaksesi tekstitiedoston kirjoittamisen Arduinoilla, sinun tulee ensin luoda uusi tiedosto projektisi hakemistoon. Käynnistä Arduino-ohjelmointiympäristö ja avaa tiedosto-valikko. Valitse "Uusi tiedosto" ja anna tiedostollesi nimi. Voit kirjoittaa tekstitiedoston sisällön käyttämällä Serial.print()-funktiota. Esimerkiksi:

```Arduino
Serial.print("Tämä on tekstitiedoston sisältö!");
```

Ohjelmoitu arduino laite tulostaisi nyt tekstitiedoston sisällön sarjaporttiin, joten voit tarkistaa sen toiminnan. Voit myös käyttää Serial.println()-funktiota luodaksesi uuden rivin tekstitiedostossa.

## Syvempää tietoa

Arduinon Serial.print()-funktio ottaa vastaan useita parametreja, kuten tekstin lisäksi myös numeroita, muuttujia ja muita arvoja. Näiden avulla voit luoda monipuolisempia tekstitiedostoja. Voit myös käyttää muita tiedostojärjestelmään liittyviä funktioita, kuten open(), write() ja close(), luodaksesi ja manipuloidaksesi tekstitiedostoja projektissasi.

## Katso myös

- [Arduino Reference - Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Reference - Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino Reference - Files library](https://www.arduino.cc/en/Reference/Files)
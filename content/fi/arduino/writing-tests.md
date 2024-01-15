---
title:                "Testien kirjoittaminen"
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaisi kirjoittaa testejä Arduino-ohjelmillesi? Yksinkertaisesti sanottuna, testit auttavat varmistamaan, että koodisi toimii oikein ja välttämään mahdollisia virheitä.

## Miten

Aluksi sinun tulee asentaa testikirjasto Arduino-ympäristöösi. Voit tehdä tämän valitsemalla "Manage Libraries" ("Hallitse kirjastoja") Tools-valikosta ja etsimällä "ArduinoUnit" kirjaston. Asenna kirjasto ja käynnistä Arduino-ympäristö uudelleen.

Nyt voit aloittaa testien kirjoittamisen. Kirjoita testikoodi setup-funktioon ja määritä sen lopuksi run-tests-funktioon. Alla on yksinkertainen esimerkki testistä näppäimistön painallusta varten:

```Arduino
#include <ArduinoUnit.h>

void setup() {
  test(keypressTest);
  runTests();
}

test(keypressTest) {
  Keyboard.press('A');
  assertEqual('A', Keyboard.read());
}
```

Voit suorittaa testin valitsemalla "Verify and Upload (Compile and Upload)" Tools-valikosta. Jos kaikki menee hyvin, testi ilmoittaa "PASS" ("ONNISTUI"). Jos taas jokin menee pieleen, testi ilmoittaa "FAIL" ("EPÄONNISTUI") ja antaa tarkemman virheilmoituksen.

## Syventävä sukellus

Voit myös käyttää muita testikirjastoja, kuten Unity-testikirjastoa, joka tarjoaa enemmän ominaisuuksia ja joustavuutta. Voit myös suorittaa testejä suoraan Arduinolla käyttämällä Serial.print()-komentoja ja tarkkailla tulostusta sarjaportin kautta.

Testejä kirjoittaessa on hyvä ottaa huomioon myös testaamisen periaatteet, kuten yksikkötestaus ja testien luotettavuus. Näiden avulla varmistat, että testit ovat tehokkaita ja luotettavia.

## Katso myös

- [ArduinoUnit library documentation](https://www.arduino.cc/en/Reference/ArduinoUnit)
- [Unity Arduino library](https://github.com/ThrowTheSwitch/Unity)
- [Arduino Testing for Beginners](https://randrineer.github.io/arduino/testing/2016/04/05/arduino-testing-for-beginners.html)
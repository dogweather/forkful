---
title:                "Arduino: Testien kirjoittaminen"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Testien kirjoittaminen on tärkeä osa Arduino-ohjelmoinnin prosessia. Ne auttavat varmistamaan koodin toimivuuden ja vähentävät mahdollisten bugien määrää ohjelmassa.

## Miten
```Arduino
#include <Arduino.h>
#include <unity.h>

int add(int x, int y) {
  return x + y;
}

void test_add() {
  TEST_ASSERT_EQUAL(5, add(2, 3));
}

void setup() {
  UNITY_BEGIN();
  RUN_TEST(test_add);
  UNITY_END();
}

void loop() {}
```

### Selitys
Ensimmäisessä rivissä tuodaan tarvittavat kirjastot testien käyttöä varten. Seuraavaksi luodaan funktio, joka toimii testattavana koodina. Tässä esimerkissä funktio `add` laskee kahden luvun summan ja palauttaa sen arvon. Sitten luodaan testifunktio, joka kutsuu `TEST_ASSERT_EQUAL` funktiota. Tämä funktio vertaa kahden parametrina annetun arvon samuutta ja tulostaa testeihin liittyvän informaation tarvittaessa. Lopuksi, setup funktiossa aloitetaan Unity-testiä kutsuen `UNITY_BEGIN()` funktiota, suoritetaan testifunktio kopkaamalla `RUN_TEST()` funktiota ja lopetetaan suoritus `UNITY_END()` funktion avulla.

## Syvemmälle
Testien kirjoittaminen on hyödyllistä, kun halutaan tehdä kattavaa ja laadukasta koodia. Ne auttavat tekemään koodin toimivuudesta luotettavaa ja havaitsemaan mahdolliset virheet ennen kuin ne tulevat ongelmaksi käytettäessä oikeassa ympäristössä. Näiden etujen lisäksi testien kirjoittaminen voi nopeuttaa kehitysprosessia, sillä testien avulla voidaan nopeasti tarkistaa ohjelman toimivuus ja korjata mahdolliset ongelmat ennen kuin ne aiheuttavat suurempia ongelmia. Toisin sanoen, testien kirjoittaminen auttaa parantamaan ohjelman yleistä laatua ja vähentämään kehitykseen liittyviä riskejä.

## Katso myös
- [Unity - Arduino ohjelmointi ja yksikkötestaus](https://docs.unity3d.com/560/Documentation/Manual/testing-arduino-artifacts.html)
- [Arduino testiversio](https://github.com/adafruit/arduino-test-suite)
- [Arduino yksikkötestaus - kattavat ohjeet](https://www.arduino.cc/en/Guide/UnitTesting)
- [Junit - yksikkötestaus Java-sovelluksissa](https://junit.org/junit5/docs/current/user-guide/)
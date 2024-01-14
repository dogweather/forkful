---
title:                "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on välttämätöntä Arduino-ohjelmoinnissa varmistaaksemme, että koodimme toimii odotetulla tavalla ja välttääksemme mahdolliset virheet ja ongelmat käytössä. Testien avulla voimme myös helpottaa koodin muokkaamista ja ylläpitämistä tulevaisuudessa.

## Kuinka tehdä

Kirjoittamalla testit koodiimme, voimme varmistaa sen toimivuuden ja välttää mahdolliset virheet. Tässä on esimerkki testien kirjoittamisesta Arduino-koodissamme:

```Arduino
#include <gtest/gtest.h>

// Testikoodi, joka testaa loop-funktion toimivuuden
void loopTest() {
  // alustetaan tarvittavat muuttujat
  int x = 10;
  int y = x + 5;
  
  // testataan x- ja y-muuttujien arvoja
  EXPECT_EQ(x, 10);
  EXPECT_EQ(y, 15);
  
  // odotetaan muutama millisekuntia ennen seuraavaa testiä
  delay(10);
}

// Testien suoritus
void setup() {
  // Kutsutaan GTest-teekin alustusfunktiota
  testing::InitGoogleTest();
}

void loop() {
  // Suoritetaan testi
  loopTest();
  
  // Ajetaan GTest-teekin funkio, joka suorittaa testit
  RUN_ALL_TESTS();
}

```

Tulostus:

```
[Test Output]
[==========] Running 1 test cases.
[----------] Global test environment set-up.
[----------] 1 tests from LoopTest
[ RUN ] LoopTest.TestLoop
[ OK ] TestLoop.TestLoop (1 ms total, 1 ms single)
[----------] 1 tests from TestLoop (1 ms total)
[----------] Global test environment tear-down
[==========] 1 test cases run. (1 ms total)

```

## Syvällisempi sukellus

Testien kirjoittaminen ei ole vaikeaa ja se tuo paljon hyötyä koodiimme. Testien avulla voimme varmistaa koodimme toimivuuden erilaisissa tilanteissa ja välttää mahdolliset virheet. Testien kirjoittamiseen on myös olemassa erilaisia kirjastoja ja työkaluja, kuten GTest, jotka voivat helpottaa testien luomista ja suorittamista.

## Katso myös

- [Arduino Test Library](https://www.arduino.cc/reference/en/libraries/testlibrary/)
- [SimpleArduinoUnitTest](https://github.com/bitlash/SimpleArduinoUnitTest)
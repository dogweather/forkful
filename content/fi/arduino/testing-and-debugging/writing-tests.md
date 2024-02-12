---
title:                "Testien kirjoittaminen"
aliases:
- /fi/arduino/writing-tests.md
date:                  2024-02-03T19:29:40.752498-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Testien kirjoittaminen Arduino-ympäristössä tarkoittaa automatisoitujen testien luomista, jotka varmistavat koodisi toimivuuden Arduino-laitteissa. Ohjelmoijat tekevät näin varmistaakseen, että heidän koodinsa toimii odotetusti, vähentää virheitä ja parantaa projektien laatua, mikä on erityisen tärkeää sulautetuissa järjestelmissä, joissa vianetsintä voi olla haastavampaa.

## Kuinka:

Arduino ei sisällä valmiina testausalustaa kuten jotkut muut ohjelmointiympäristöt. Voit kuitenkin käyttää kolmannen osapuolen kirjastoja, kuten `AUnit`-kirjastoa, Arduino-koodin yksikkötestaukseen. AUnit on saanut inspiraationsa Arduinon sisäänrakennetusta kirjastosta, `ArduinoUnit`, ja Googlen testausalustasta, `Google Test`.

### Esimerkki AUnitin kanssa:

Asenna ensin AUnit Kirjastohallinnan kautta Arduino IDE:ssä: mene kohtaan Sketch > Include Library > Manage Libraries... > etsi AUnit ja asenna se.

Sen jälkeen voit kirjoittaa testejä näin:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Tyhjä
}
```
Tämän testin lataamisen jälkeen Arduino-laudallesi, avaa sarjamonitori nähdäksesi testitulokset. Näet tulosteessa, läpäisikö jokainen testi vai epäonnistuiko:

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

Tämä yksinkertainen esimerkki havainnollistaa AUnitin käyttöä LED-pinnin tilan testaamisessa. Testien luomisen avulla voit vahvistaa, että Arduinosi käyttäytyy odotetusti eri olosuhteissa. AUnitin avulla voit kirjoittaa monimutkaisempia testejä, testisarjoja ja nauttia ominaisuuksista, kuten testien aikakatkaisut ja valmistelu/purkutoimenpiteet monimutkaisempiin skenaarioihin.

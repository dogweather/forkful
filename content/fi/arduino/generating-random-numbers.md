---
title:                "Satunnaislukujen luominen"
html_title:           "Arduino: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Arduinolla satunnaislukujen generointi voi olla hyödyllistä esimerkiksi pelien tai arpajaisten luomisessa, satunnaisten päätösten tekemisessä tai salauksen avaimien generoinnissa.

## Miten

**Huom. Tämä ohje on toteutettu käyttäen Arduino UNO -laitetta.**

Satunnaislukujen generoiminen Arduinoilla onnistuu hyödyntämällä laitteen analogista sisäänmenoa ja sen ADC-muunninta. Tässä esimerkissä luomme funktiopakkauksen, joka palauttaa satunnaisen luvun väliltä 0-255.

```Arduino
// Satunnaisen luvun generointi väliltä 0-255
#include <Arduino.h>

// Funktiopakkaus, joka palauttaa satunnaisen luvun
// Parametrina käytetään analogisen sisääntulon pinniä
int satunnainenLuku(int analogPin) {
  // Asetetaan analogisen sisääntulon pinni INPUT-tilaan
  pinMode(analogPin, INPUT);
  
  // Luetaan pinnin laskennallinen jännite ja tallennetaan se muuttujaan
  int analoginenArvo = analogRead(analogPin);
  
  // Muutetaan laskennallinen jännite välille 0-255 ja palautetaan se
  return map(analoginenArvo, 0, 1023, 0, 255);
}

// Pääohjelma
void setup() {
  // Alustetaan sarjaliikenne
  Serial.begin(9600);
}

// Päälooppi, jossa silmukka jatkuu ikuisesti
void loop() {
  // Kutsutaan satunnainenLuku-funktiota ja tulostetaan arvo
  int randomValue = satunnainenLuku(A0);
  Serial.println(randomValue);

  // Lyhyt viive ennen seuraavan arvon generointia
  delay(1000);
}
```

**Sample output:**

```
159
112
45
78
203
14
```

## Syväsukellus

Arduino käyttää Pseudorandom Number Generator (PRNG) -algoritmia satunnaislukujen generoimiseen. Tämä tarkoittaa sitä, että luvut eivät ole täysin satunnaisia, vaan ne voidaan toistaa tietyillä syötteillä. PRNG-algoritmi perustuu matemaattiseen kaavaan, joka tuottaa pseudo satunnaisia arvoja.

Arduinoissa on myös mahdollista käyttää laitteen sisäistä satunnaislukugeneraattoria käyttäen randomSeed() -funktiota. Tämä on luotettavampi tapa generoida satunnaisia lukuja, sillä se perustuu laitteen ympäristön muuttujiin, kuten kelloaikaan ja lämpötilaan. Tämä tekee satunnaislukugeneraattorista arvauksia vaikeamman.

## Katso myös

- [Arduino Reference - Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Playground - Random](https://playground.arduino.cc/Main/Random/)
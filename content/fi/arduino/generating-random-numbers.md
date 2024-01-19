---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen luominen liittyy epäsäännöllisyyksien tai ennakoimattomien tulosten luomiseen ohjelmassa. Ohjelmoijat käyttävät sitä jakaakseen resursseja, testatakseen ohjelmistoja tai pelien kehittämisessä.

## Näin se tehdään:
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  Serial.println(random(100)); // Tulostaa satunnaisluvun väliltä 0-99
  delay(1000);
}
```
Koodiesimerkissä Arduino lukee dataa analogisesta pinnistä A0 ja käyttää sitä randomSeed() funktion lukemaan, mikä asettaa satunnaislukugeneraattorin alkutileeksi. Tämä tulee luomaan todellisen satunnaisluvut.

## Syvempi tutkimus
Satunnaislukujen luomisen historia ulottuu kauas kreikkalaiseen ajanlaskuun, jossa erilaisia menetelmiä käytettiin arpakuutioiden heittoon. Sittemmin ohjelmistoissa on kehitetty useita erilaisia algoritmeja, joista kaikkea ei edes voida käsitellä tässä artikkelissa. Arduinon mallissa käytetään lineaarista kongruenssimenetelmää. Se on melko yksinkertainen, mutta tarpeeksi monipuolinen useimpiin tarkoituksiin. Kuitenkin huomioitavaa on, että se ei ole riittävän turvallinen kryptografian näkökulmasta, joten sen käyttö turvallisuuskriittisissä sovelluksissa ei ole suositeltavaa.

## Katso myös
Voit syventää tietämystäsi satunnaislukujen luomisesta ja eri menetelmistä seuraavien linkkien kautta:
1. [Random Number Generation in C++](https://www.cplusplus.com/reference/random/)
2. [Generating random numbers in Python](https://docs.python.org/3/library/random.html)
3. [Understanding Random Numbers in JavaScript](https://www.javascripttutorial.net/javascript-random/)

Päätimme tässä vaiheessa, koska tämä artikkeli on tarkoitettu olemaan epämuodollinen ja suoraviivainen introduktio Arduino-ohjelmointiin suunnattuna suomalaisille lukijoille. Toivottavasti nautit oppimasta!
---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:48:37.569230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? | Mitä & Miksi?
Satunnaisluvut ovat arvaamattomia numeroita. Käytämme niitä simuloimaan sattumanvaraisuutta ja luomaan arvaamattomuutta ohjelmiin – esimerkiksi peleissä tai turvallisuuskäytännöissä.

## How to: | Kuinka:
```Arduino
void setup() {
  Serial.begin(9600);          // Käynnistetään sarjaportti
  randomSeed(analogRead(0));   // Alustetaan satunnaislukugeneraattori
}

void loop() {
  int randomValue = random(100);  // Arvotaan numero väliltä 0-99
  Serial.println(randomValue);    // Tulostetaan se sarjaporttiin
  delay(1000);                    // Odota sekunti ennen seuraavaa arvonantoa
}
```
### Sample output:
```
45
23
77
...
```

## Deep Dive | Syvä Sukellus:
Arduino käyttää pseudo-satunnaislukugeneraattoria (PRNG) lukujen arvontaan. Se ei ole oikeasti täysin satunnainen, se vain näyttää siltä. Menneisyydessä heikko satunnaisuus saattoi aiheuttaa ongelmia, esimerkiksi turvallisuusriskejä. Vaihtoehtoja on monia, kuten parempia PRNG-algoritmeja tai fyysisiä satunnaislukugeneraattoreita. `randomSeed()`-funktion käyttö on tärkeää, ettei joka kerta käynnistyessä saada samaa "satunnais" sarjaa.

## See Also | Katso Myös:
- [Arduino Reference: random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Reference: randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Wikipedia: Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)

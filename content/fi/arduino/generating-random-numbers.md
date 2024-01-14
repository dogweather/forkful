---
title:                "Arduino: Satunnaislukujen generointi"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Kuvittele, että sinulla on Arduino-projekti, joka vaatii satunnaisia numeroita. Ehkä haluat simulaattorin tai arpajaisen pienen pelin osaksi. Tässä tapauksessa random number generator on juuri se mitä tarvitset! Se on hyödyllinen ja hauska tapa lisätä arvaamattomuutta ja mielenkiintoa projektiisi.

## Kuinka

Koodin kirjoittaminen on helppoa ja nopeaa Arduino-ohjelmointiympäristössä. Vain muutamalla rivillä koodia voit luoda random number generatorin:

```Arduino
int randomNum = random(0, 10); // Generoi satunnaisen numeron välillä 0 ja 10

Serial.println(randomNum); // Tulostaa satunnaisen numeron sarjaporttiin
```

Tässä esimerkissä käytetään `random()` -funktiota, joka ottaa kaksi parametria: minimi- ja maksimiarvot halutulle numeron välille. Voit muuttaa näitä lukuja tarpeidesi mukaan.

Voit myös käyttää `randomSeed()` -funktiota, jos haluat asettaa oman alkunumerosi. Tämä on hyödyllistä esimerkiksi, kun haluat generoida samat satunnaiset numerot jokaisella käynnistyksellä.

```Arduino
randomSeed(123); // Asettaa alkunumeron 123:ksi
int randomNum = random(0, 10); // Generoi uuden satunnaisen numeron välillä 0 ja 10, mutta se pysyy samana jokaisella käynnistyksellä
```

## Syvempi sukellus

Random number generatorin taustalla on pseudorandom-algoritmi, joka tuottaa tiettyä logiikkaa noudattaen satunnaisia numeroita. Tämän vuoksi vaikka tuloksena olevat numerot ovatkin sattumanvaraisia, ne eivät ole täysin arvaamattomia. Tämä on tärkeää huomioida, jos luot esimerkiksi arpajaispelin, jossa tarvitaan täydellistä arvaamattomuutta.

Voit myös tutkia muita `random()` -funktion vaihtoehtoja, kuten `randomSeed()` -funktion jatkoksi luotuja `srandom()` ja `randomize()` -funktioita. Näiden käyttö voi mahdollistaa erilaisten arvojen generoimisen ja mahdollisesti paremman satunnaisuuden.

## Katso myös

- [Arduino Reference: random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Blogipostaus: Tehtävä 1 – Random numero mikrokontrollerilla](https://roionalts.wordpress.com/2013/06/04/tehtava-1-random-numero-mikrokontrollerilla/)
- [Arduino Random Numbers: Building a Better Random Function](https://forum.arduino.cc/index.php?topic=437149.0)
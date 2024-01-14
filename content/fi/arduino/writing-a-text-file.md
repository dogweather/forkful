---
title:    "Arduino: Tekstitiedoston kirjoittaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen voi olla hyödyllistä, kun haluat tallentaa tietoa tai viestejä ohjelman suorittamisen aikana. Tekstitiedosto on yksi tapa tallentaa tätä tietoa ja palauttaa se myöhemmin tarvittaessa.

## Miten

Kirjoittaminen teksti-tiedostoon Arduino-ohjelmassa on helppoa. Ensimmäinen vaihe on avata tiedosto, jonka avulla voit kirjoittaa tietoa. Voit tehdä tämän käyttämällä `SD.open()`-funktiota. Seuraavaksi voit käyttää `println()`-funktiota kirjoittaaksesi haluamasi tiedot.

```Arduino
#include <SD.h>

File tiedosto;

void setup() {
  tiedosto = SD.open("data.txt", FILE_WRITE);
  if (tiedosto) {
    tiedosto.println("Tämä on esimerkki tekstiä.");
    tiedosto.println("Tässä on toinen esimerkki.");
    tiedosto.close();
  }
}

void loop() {

}
```

Yllä olevassa esimerkissä avataan tiedosto nimeltä "data.txt" ja kirjoitetaan siihen kaksi riviä tekstiä. Muista myös sulkea tiedosto `close()`-funktiolla.

## Syvemmälle

Voit myös käyttää `print()`-funktiota kirjoittaaksesi merkkijonoja tai muuttujia ilman rivinvaihtoa. Voit sitten käyttää `println()`-funktiota lopuksi kirjoittaaksesi uuden rivin.

Voit myös lukea tietoa teksti-tiedostosta käyttämällä `read()`-funktiota. Voit antaa sille parametrin, joka määrittelee kuinka monta merkkiä haluat lukea. Esimerkiksi `read(5)` lukee viisi ensimmäistä merkkiä tiedostosta.

## Katso myös

Tässä muutamia hyödyllisiä linkkejä lisätietoa varten:

- Arduino SD-kirjaston dokumentaatio: https://www.arduino.cc/en/Reference/SD
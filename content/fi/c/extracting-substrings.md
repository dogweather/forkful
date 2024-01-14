---
title:                "C: Alialaisten erottaminen."
simple_title:         "Alialaisten erottaminen."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

On monia erilaisia käyttötarkoituksia, jotka voivat tarvita merkkijonojen pilkkomista tai alimerkkijonon irrottamista tietyistä merkkijonoista. Tämä voi sisältää esimerkiksi tietokantaoperaatioita, tekstinkäsittelyä tai käyttäjän syötteen validointia.

## Miten

Merkkijonon alimerkkiä voidaan irrottaa käyttämällä C-kielen `strncpy()` -funktiota. Seuraavassa esimerkissä irrotamme alimerkkijonon "World" merkkijonosta "Hello World" ja tulostamme sen konsoliin.

```C
include <stdio.h>
include <string.h>

int main() {

    // Alkuperäinen merkkijono
    char string[] = "Hello World";
    // Alustetaan muuttujat uutta merkkijonoa varten
    char substring[6];
    int start = 6; // Indeksi, josta aloitetaan alimerkkijonon irrottaminen
    int length = 5; // Alimerkkijonon pituus

    // Käytetään strncpy() funktiota alimerkkijonon irrottamiseen
    strncpy(substring, string + start, length); // substring = "World"
    // Lisätään loppuun C-merkkijonon päättävä nolla
    substring[length] = '\0';

    // Tulostetaan alimerkkijono konsoliin
    printf("%s", substring);

    return 0;
}
```

Tämän koodin antama tulos on "World".

## Syvällinen tarkastelu

Mikäli alimerkkijonon pituus ei ole tiedossa etukäteen, voimme käyttää C-kielen `strlen()` -funktiota sen määrittämiseen. `strncpy()`-funktion sijaan voimme myös käyttää `memcpy()`-funktiota, joka toimii samalla tavalla. Tärkeää on myös varmistaa, että lopulliseen merkkijonoon lisätään C-merkkijonon päättävä nolla `'\0'`, jotta se pysyy kelvollisena C-merkkijonona.

## Katso myös

- [strncpy() oheinen dokumentaatio C-programmointikielen virallisessa referenssissä](https://devdocs.io/c/string/byte/strncpy)
- [memcpy() oheinen dokumentaatio C-programmointikielen virallisessa referenssissä](https://devdocs.io/c/string/byte/memcpy)
- [strlen() oheinen dokumentaatio C-programmointikielen virallisessa referenssissä](https://devdocs.io/c/string/byte/strlen)
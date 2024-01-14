---
title:                "C: Alimerkkijonojen erottaminen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on olennainen osa monien ohjelmien kehitystä, sillä se tarjoaa mahdollisuuden käsitellä ja manipuloida tekstipohjaisia tietoja. Tämä tekniikka voi olla hyödyllinen esimerkiksi tietojen käsittelyssä, tiedostojen luomisessa tai jopa tekstianimaatioissa.

## Miten tehdä

Substringien erottaminen on melko yksinkertaista C-ohjelmointikielessä, ja siihen tarvitaan vain muutama perustoiminto. Esimerkiksi, jos haluamme tulostaa merkkijonon viisi ensimmäistä merkkiä, voimme käyttää funktiota `strncpy()` seuraavasti:

``` C
#include <stdio.h>
#include <string.h>

int main() {
   char string[20] = "Tervetuloa";
   char substring[6];

   strncpy(substring, string, 5);
   substring[5] = '\0';

   printf("Substring: %s\n", substring);

   return 0;
}
```

Tämä koodi tulostaa "Terve" ja osoittaa, kuinka voimme käyttää `strncpy()` -funktiota yksinkertaisen substringin tarkkailuun.

## Syvällisempi tarkastelu

Monet C-kirjastot tarjoavat joukon toimintoja, jotka helpottavat substringien erottamista ja käsittelyä. Esimerkiksi `strstr()`-funktio löytää ensimmäisen esiintymän tiettynä merkkijonona ja `strchr()`-funktio löytää ensimmäisen esiintymän tiettynä merkkinä.

On myös tärkeää muistaa, että C-kielessä merkkijonot voidaan käsitellä merkkijonomuuttujina (char arrays) tai osoittimina (pointers). Tämä tarkoittaa, että voimme käyttää erilaisia käytäntöjä substringien erottamisessa, kuten merkkijonojen käsittelemistä tai osoittimien käyttöä.

## Katso myös

- [C-kielen virallinen dokumentaatio](https://en.cppreference.com/w/c)
- [Monipuolinen opas C-ohjelmointikieleen](https://www.geeksforgeeks.org/c-programming-language/)
- [Substringien erottamisen perusteet Javascriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
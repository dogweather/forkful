---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "C: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa meidän täytyy selvittää merkkijonon pituus ohjelmassamme. Se voi olla tarpeen esimerkiksi tietojen käsittelyssä tai merkkijonojen vertailussa. Siksi onkin tärkeää tietää, miten tämä tehdään C-kielellä.

## Miten tehdä

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char str[] = "Tämä on esimerkkimerkkijono";
    int length = strlen(str); // käytetään strlen-funktiota
    printf("Merkkijonon pituus on: %d", length);
    return 0;
}
```
Tässä esimerkissä käytämme C:n `strlen`-funktiota, joka lukee merkkijonon ja palauttaa sen pituuden. Ensiksi täytyy kuitenkin sisällyttää `string.h` kirjasto ohjelmaamme, jotta voimme käyttää tätä funktiota. Tämän jälkeen luodaan merkkijono `str` ja lasketaan sen pituus `strlen`-funktiolla. Lopuksi tulostetaan pituus `printf`-funktiolla.

**Tulostus:**
```
Merkkijonon pituus on: 27
```

## Syvemmälle sukellettaessa

C-kielessä merkkijonoja käsitellään taulukkoina, ja jokaisella merkillä on oma ASCII-koodinsa. Kun ohjelma lukee merkkijonon, se käy läpi merkit yksi kerrallaan ja laskee pituuden. Näin ollen `strlen`-funktiolla ei ole samaa toimintaa kuin esimerkiksi `sizeof`-funktiolla, joka laskee muuttujan koon tavuina.

Lisäksi on hyvä huomata, että `strlen` ei laske mukaan null-terminatoria eli merkkijonon lopussa olevaa `\0` merkkiä. Tämä täytyy ottaa huomioon laskettaessa merkkijonon pituutta.

## Katso myös

- [C-kielen opas](https://www.cprogramming.com/tutorial/c-tutorial.html)
- [Merkkijonojen käsittely C-kielessä](https://www.programiz.com/c-programming/c-strings)
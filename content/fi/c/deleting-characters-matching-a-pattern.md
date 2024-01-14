---
title:                "C: Kuviota vastaavien merkkien poistaminen"
simple_title:         "Kuviota vastaavien merkkien poistaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointiprosessissa voi esiintyä tarvetta poistaa merkkejä, jotka täsmäävät tiettyyn kaavaan. Tämä voi olla osa isompaa ohjelmointitehtävää tai yksinkertainen ratkaisu tietyn ongelman korjaamiseksi. Seuraavassa käsitellään lyhyesti, miten tämän voi tehdä C-kielellä.

## Miten

C-kielessä on monia tapoja poistaa merkkejä, jotka osuvat tiettyyn kuvioon. Yksi yleisimmistä tavoista on käyttää standardikirjaston "string.h" -funktiota "strchr", joka löytää ensimmäisen esiintymän annetusta merkistöstä. Tämän jälkeen voidaan käyttää "strcpy" -funktiota, joka kopioi merkkijonon annettuun puskuriin ja jättää halutut merkit pois. Tässä on yksinkertainen esimerkki, joka poistaa kaikki 'a' merkit annetusta merkkijonosta:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    // alustetaan merkkijono
    char s[] = "Hei kaikki!";

    // poistetaan merkit
    char* result = strchr(s, 'a'); // löydetään ensimmäinen esiintymä 'a' merkille
    if (result != NULL) { // jos merkki löytyi
        strcpy(result, result+1); // kopioi merkkijono alkamisesta merkin jälkeen
    }

    // tulostetaan lopputulos
    printf("%s\n",s); // tulostaa "Hei kikki!"

    return 0;
}
```

## Syvällisemmin

Yllä olevassa esimerkissä käytetään vain yhtä merkkiä, mutta voit myös etsiä ja poistaa pidempiä merkkijonoja käyttämällä "strstr" -funktiota. Voit myös käyttää "strchr" ja "strcpy" -funktioita loopin sisällä, jotta voit poistaa kaikki halutut merkit merkkijonosta. On myös muita tapoja saavuttaa sama tulos, esimerkiksi käyttämällä "memmove" -funktiota, joka siirtää merkkijonon osia toisiin paikkoihin merkki kerrallaan.

## Katso myös

- [string.h tutkimusohjelma](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C string -opas](https://www.programiz.com/c-programming/c-strings)
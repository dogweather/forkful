---
title:                "C: Päivämäärän saaminen"
simple_title:         "Päivämäärän saaminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi Päivämäärän Haku Kannattaa?

Päivämäärän haku on olennainen osa useimpien ohjelmien ja sovellusten toimintaa. Se mahdollistaa ajankohtaisen tiedon esittämisen ja käyttäjäkokemuksen parantamisen. Ilman päivämäärän hakua, ohjelmat olisivat vanhentuneita ja mahdollisesti jopa virheellisiä.

## Kuinka Päivämäärä Haetaan?

Päivämäärän hakeminen C-ohjelmassa on helppoa. Se vaatii vain muutaman rivin koodia ja C-kirjaston käyttöä. Alla on esimerkki koodista, joka hakee ja tulostaa nykyisen päivämäärän konsoliin.

```C
#include <stdio.h> 
#include <time.h> 

int main() 
{ 
    // Hakee nykyisen ajan ja tallentaa sen tietorakenteeseen 
    time_t nykyinen_aika = time(NULL); 

    // Muuttaa ajan paikalliseen aikamuotoon 
    struct tm *paikallinen_aika = localtime(&nykyinen_aika); 

    // Tulostaa päivämäärän konsoliin 
    printf("Nykyinen päivämäärä on %s", asctime(paikallinen_aika)); 

    return 0; 
} 
```

Ohjelman tuloste:

```
Nykyinen päivämäärä on Mon Aug 30 13:24:13 2021
```

## Syvempi Päivämäärän Haku

Päivämäärän hakeminen käyttäen C-kieltä voi olla joustavaa ja monipuolista. C-kirjastossa on useita funktioita, jotka mahdollistavat päivämäärän tarkemman määrittelyn ja muokkaamisen. Esimerkiksi `strftime()`-funktio mahdollistaa päivämäärän muotoilun halutunmukaisesti. Lisäksi voidaan käyttää erilaisia aikatietorakenteita, kuten `struct tm`, joka sisältää tietoa vuodesta, kuukaudesta, päivästä jne.

## Katso Myös

- [C-kirjaston ajan käsittely](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C-kielen ohjelmointiopas](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Tutorial: Getting Current Date and Time in C](https://www.programiz.com/c-programming/examples/current-date-time)
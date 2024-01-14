---
title:    "C: Kahden päivämäärän vertailu"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää?

Vertaillessamme kahta päivämäärää voimme selvittää, onko jokin tapahtuma tapahtunut ennen vai jälkeen toisen tapahtuman. Tämä on erittäin hyödyllistä esimerkiksi varausjärjestelmien tai tehtävälistojen yhteydessä.

## Kuinka vertailla

Käyttämällä C-kielen aikafunktioita, voimme luoda kaksi päivämäärää ja verrata niitä toisiinsa käyttämällä vertailuoperaattoreita. Koodin esimerkissä luomme kaksi päivämäärää ja tarkistamme, kumpi niistä on aikaisempi. Alla olevassa koodiesimerkissä käytetään aikafunktioita `mktime` ja `difftime`.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Luodaan ensimmäinen päivämäärä
    struct tm date1 = {0};
    date1.tm_year = 2020;
    date1.tm_mon = 0;
    date1.tm_mday = 1;

    // Luodaan toinen päivämäärä
    struct tm date2 = {0};
    date2.tm_year = 2020;
    date2.tm_mon = 6;
    date2.tm_mday = 1;

    // Vertaillaan päivämääriä
    if (difftime(mktime(&date1), mktime(&date2)) > 0)
    {
        printf("Ensimmäinen päivämäärä on aikaisempi.");
    }
    else
    {
        printf("Toinen päivämäärä on aikaisempi.");
    }

    return 0;
}
```

Tulostus:

```
Toinen päivämäärä on aikaisempi.
```

## Syvä sukellus

Vaikka kaksi päivämäärää näyttävät samalta, ne ovat todellisuudessa erilaisia arvoja, joita tietokone käsittelee. Päivämäärät tallennetaan Unix-aikana, joka on käytännössä sekunteina kuluneesta ajasta 1. tammikuuta 1970 klo 00.00 UTC:sta lähtien. Tämä tarkoittaa, että kun luomme päivämääriä, tietokone muuntaa ne nanosekunneiksi ja tallentaa ne yhtenäiseen muotoon, jota on helppo vertailla.

Yksi tärkeä huomioitava asia on, että päivämäärän vertailun on oltava samassa aikavyöhykkeessä, jotta se toimisi oikein. Jos esimerkiksi vertailemme päivämäärää Suomen aikavyöhykkeellä ja Yhdysvaltojen aikavyöhykkeellä, tietokone laskee eroa ajanerojen lisäksi, mikä saattaa johtaa virheellisiin tuloksiin.

# Katso myös

- [Aikafunktiot C-kielessä (cprogramming.com)](https://www.cprogramming.com/tutorial/time.html)
- [Aikafunktiot C-kirjastossa (C library functions)](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Päivämäärän vertailu Java-kielellä (Stack Overflow)](https://stackoverflow.com/questions/22359911/compare-two-dates-in-java)
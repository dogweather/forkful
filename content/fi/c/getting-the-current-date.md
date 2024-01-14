---
title:                "C: Päivämäärän hakeminen"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Tämän päivän päivämäärän saaminen C-ohjelmassa saattaa tuntua yksinkertaiselta tehtävältä, mutta sillä on todellisuudessa tärkeä käyttötarkoitus. Päivämäärän käyttäminen ohjelmassa voi auttaa seuraamaan aikaa ja päivittämään automaattisesti tietyt toiminnot, kuten tiedostojen tallentaminen päivämäärän mukaan.

## Miten

Tämän tavoitteen saavuttamiseksi käytämme C-kielellä valmiina sisäänrakennettua funktiota nimeltä "time ()." Tämä funktio palauttaa ajan lukuna, joka on kulunut tietystä päivästä tiettyyn häviämiseen. Tämän funktion paluuarvo tallennetaan muuttujaan, jossa päivämäärän tiedot on tallennettu.

```C
#include <stdio.h>
#include <time.h>

int main()
{
   time_t nyt;
   time(&nyt);

   printf("Tänään on %s", ctime(&nyt));
   return 0;
}
```

Tässä esimerkissä käytämme funktiota "time ()" saadaksemme nykyisen ajan ja tallentamaan sen muuttujaan "nyt." Sitten käytämme funktiota "ctime ()", joka muuntaa ajanluvun merkkijonoksi ja tulostaa sen konsoliin.

**Output:**

Tänään on Wed Mar 10 20:26:26 2021

## Syventävä sukellus

Time-funktiolla on myös muita hyödyllisiä ominaisuuksia, kuten mahdollisuus muuntaa aika paikallisesti käytettäväksi ajanvyöhykkeeksi tai tulostaa aika tiettynä muotona. Voit tutkia lisää Time-funktion dokumentaatiota saadaksesi lisätietoja ja käyttövinkkejä.

## Katso myös

- [C-funktion Time dokumentaatio](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Vinkkejä ja temppuja päivämäärän ja ajan käsittelyyn C-kielellä](https://www.dreamincode.net/forums/topic/133909-working-with-dates-in-c-tips-and-tricks/) 
- [Time and Date -opetusohjelma C-kielellä](https://www.programiz.com/c-programming/c-date-time)
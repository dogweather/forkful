---
title:    "C: Tämänhetkisen päivämäärän saaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voi olla tarpeen saada tietoja nykyisestä päivästä ohjelmassa. Jotkut esimerkit ovat kalenteritoimintojen toteuttaminen, tapahtumien seuraaminen tai vain tiedon tallentaminen tietokantaan.

## Kuinka

C-kielessä on useita tapoja saada nykyinen päivämäärä. Yksi tapa on käyttää time.h-kirjastoa ja sen sisältämää time() -funktiota. Tämä funktio palauttaa tietokonetaulukkoarvon, joka sisältää nykyisen päivän tiedot, kuten vuoden, kuukauden, päivän, tunnin, minuutin ja sekunnin. Alla on esimerkki tämän funktion käytöstä:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Asetetaan aika tietokoneen mukaan
    time_t t = time(NULL);
    // Muutetaan aika rakenteeseen
    struct tm tm = *localtime(&t);
    // Tulostetaan nykyinen päivämäärä muodossa pp.kk.vvvv
    printf("%d.%d.%d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    
    return 0;
}
```

Tämän koodin tulosteena saadaan nykyinen päivämäärä muodossa pp.kk.vvvv. Funktio localtime() muuntaa tietokoneen ajan paikallisajaksi riippuen aikavyöhykeasetuksista. Tämä takaa oikean päivämäärän saamisen myös eri aikavyöhykkeillä.

## Syvempi sukellus

Time.h-kirjaston sisältämien funktioiden lisäksi C-kielestä löytyy myös muita tapoja saada nykyinen päivämäärä. Esimerkiksi ISO C -standardi sisältää localtime_r() -funktion, joka on parempi versio localtime()-funktiosta monisäikeisille ohjelmille. Lisäksi C++ -kielessä voidaan käyttää boost -kirjastoa, joka tarjoaa monipuolisempia mahdollisuuksia päivämäärien käsittelyyn.

## Katso myös

- [time.h kirjaston dokumentaatio](https://www.cplusplus.com/reference/ctime/)
- [ISO C -standardin localtime_r() -funktio](https://en.cppreference.com/w/c/chrono/localtime)
- [Boost-kirjasto](https://www.boost.org/)
---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Hankitaan nykyisen päivämäärän tieto tarkoittaa järjestelmäaikaan, erityisesti päivämäärään, pääsemistä. Ohjelmoijat tekevät tämän yleensä ajastimen, aikaleiman tai päivitystarpeen määrittämiseksi.

## Näin teet:
```C
#include <stdio.h>
#include <time.h>

int main() {
    // Luodaan time_t-tyyppinen objekti
    time_t current_time;

    // Otetaan aika talteen
    time(&current_time);

    // Tulostetaan nykyinen päivämäärä
    printf("Nykyinen päivämäärä: %s", ctime(&current_time));

    return 0;
}
```
Esimerkin tulostus näyttää jotain tältä: `Nykyinen päivämäärä: Thu May 27 00:00:00 2021`.

## Syväsukellus
Alkujaan nykyisen päivämäärän saamiseen on käytetty `time()`-funktiota. Sen sijaan nykyään usein käytetään `gettimeofday()`-funktiota, joka tarjoaa sekunnin murto-osan tarkkuutta. On olemassa myös muita tapoja saada nykyinen päivämäärä, kuten kirjastot `Boost.Date_Time` tai `libC`.

Tässä esitellyssä menetelmässä käytetään `time()`-funktiota, joka palauttaa ajan sekunteina sitten UNIX Epochin (1. tammikuuta 1970, UTC). Tämä arvo tallennetaan `time_t`-tyyppiseen muuttujaan. Sitten `ctime()`-funktiota käytetään muuntamaan tämä sekuntiaika merkkijonoksi.

## Katso myös
- [Linuxin `time()`-manuaali](https://man7.org/linux/man-pages/man2/time.2.html)
- [cplusplus.com `ctime` library reference](http://www.cplusplus.com/reference/ctime/)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [UNIX Epoch Wikipedia](https://fi.wikipedia.org/wiki/Ajanlaskun_aloitus)
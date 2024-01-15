---
title:                "Kahden päivämäärän vertailu"
html_title:           "C: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä joudutaan vertailemaan kahta eri päivämäärää. Tämä voi olla tarpeen esimerkiksi tapahtumien järjestämiseen tai aikajärjestykseen perustuvien toimintojen suorittamiseen. Opi, miten voit helposti vertailla kahta päivämäärää C-ohjelmoinnin avulla.

## Kuinka

```C
#include <stdio.h>
#include <time.h>

int main() {
  
  // Luodaan kaksi aikarakennetta
  struct tm date1 = {0};
  struct tm date2 = {0};

  // Asetetaan halutut päivämäärät
  date1.tm_year = 121; // Vuosi 2021
  date1.tm_mon = 1; // Helmikuu
  date1.tm_mday = 22; // 22. päivä

  date2.tm_year = 120; // Vuosi 2020
  date2.tm_mon = 11; // Joulukuu
  date2.tm_mday = 1; // 1. päivä

  // Muunnetaan aikarakenteet aikaleimoiksi
  mktime(&date1);
  mktime(&date2);

  // Vertaillaan päivämääriä
  if (date1.tm_year > date2.tm_year) {
    printf("Ensimmäinen päivämäärä on myöhäisempi kuin toinen päivämäärä.");
  }
  else if (date1.tm_year < date2.tm_year) {
    printf("Toinen päivämäärä on myöhäisempi kuin ensimmäinen päivämäärä.");
  }
  else { // Vuodet ovat samat, vertaillaan kuukausia
    if (date1.tm_mon > date2.tm_mon) {
      printf("Ensimmäinen päivämäärä on myöhäisempi kuin toinen päivämäärä.");
    }
    else if (date1.tm_mon < date2.tm_mon) {
      printf("Toinen päivämäärä on myöhäisempi kuin ensimmäinen päivämäärä.");
    }
    else { // Kuukaudet ovat samat, vertaillaan päiviä
      if (date1.tm_mday > date2.tm_mday) {
        printf("Ensimmäinen päivämäärä on myöhäisempi kuin toinen päivämäärä.");
      }
      else if (date1.tm_mday < date2.tm_mday) {
        printf("Toinen päivämäärä on myöhäisempi kuin ensimmäinen päivämäärä.");
      }
      else { // Päivämäärät ovat samat
        printf("Päivämäärät ovat samat.");
      }
    }
  }

  return 0;
}
```

Tämä esimerkkikoodi osoittaa, miten voit vertailla kahta päivämäärää C-ohjelmoinnin avulla. Koodi hyödyntää aikarakenteita ja aikaleimoja, jotta päivämäärien vertailu onnistuu.

## Syväsukellus

Päivämäärien vertaileminen voi joskus olla monimutkaisempaa, sillä päivämäärät voivat sisältää myös aikaa. Tällöin tarvitaan esimerkiksi `difftime`-funktiota, joka vertailee kahta aikaleimaa ja palauttaa niiden välisen ajan erotuksen. Lisäksi voit käyttää `struct tm` -rakenteen muita kenttiä, kuten tunti tai minuutti, vertailuun.

## Katso myös

- [C-kielen virallinen dokumentaatio](https://www.iso.org/standard/74528.html)
- [Aikarakenne C-ohjelmoinnissa](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [Esimerkkejä päivämäärien vertailusta C:ssä](https
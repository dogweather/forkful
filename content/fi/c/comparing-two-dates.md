---
title:                "Kahden päivämäärän vertailu"
date:                  2024-01-20T17:32:39.599702-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Vertaamme kahta päivämäärää tunnistaaksemme ajanjaksoja ja järjestyksiä. Ohjelmoijat tekevät tätä aikataulujen hallintaan, ajan kulumisen seuraamiseen tai pätevyyksien voimassaolon tarkistukseen.

## How to: (Kuinka tehdään:)
```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // tm_year - vuodet ajanlaskun alusta, tm_mon 0-11, tm_mday 1-31
    if (date1.tm_year < date2.tm_year) return -1;
    if (date1.tm_year > date2.tm_year) return 1;
    if (date1.tm_mon < date2.tm_mon) return -1;
    if (date1.tm_mon > date2.tm_mon) return 1;
    if (date1.tm_mday < date2.tm_mday) return -1;
    if (date1.tm_mday > date2.tm_mday) return 1;
    return 0; // Päivämäärät ovat samat
}

int main() {
    struct tm date1 = { .tm_year=122, .tm_mon=3, .tm_mday=5 }; // 2022-04-05
    struct tm date2 = { .tm_year=122, .tm_mon=6, .tm_mday=9 }; // 2022-07-09
    
    int result = compare_dates(date1, date2);
    
    if (result < 0) {
        printf("Ensimmäinen päivämäärä on aiempi.\n");
    } else if (result > 0) {
        printf("Toinen päivämäärä on aiempi.\n");
    } else {
        printf("Päivämäärät ovat samat.\n");
    }
    return 0;
}
```
Output:
```
Ensimmäinen päivämäärä on aiempi.
```

## Deep Dive (Syväsukellus):
C-kielessä päivämäärien vertailu vaatii `struct tm` käyttöä, joka on osa `time.h`-kirjastoa. Historiallisesti, C on käsittellyt päivämäärät ja ajat yksinkertaisten rakenteiden kautta, mikä tekee vertailusta suoraviivaista, mutta rajallista. Vaihtoehtoja kuten `difftime`-funktio on käytetty sekuntien erojen laskemiseen, mutta tarkka vuosi-, kuukausi- ja päivävertailu vaatii yllä olevanlaista lähestymistä. Vertailun täsmällisyys on tärkeää, kun esimerkiksi lasketaan eräpäiviä tai tarkastetaan aikaleimoja.

## See Also (Katso myös):
- [C Library - time.h](https://www.cplusplus.com/reference/ctime/)
- [Comparing dates in ISO8601 format](https://en.wikipedia.org/wiki/ISO_8601)
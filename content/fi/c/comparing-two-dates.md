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

## Mitä ja miksi?

Vertaaminen kahden päivämäärän välillä on prosessi, jossa kaksi päivämäärää verrataan keskenään selvittääkseen, mikä niistä on aiempi tai myöhempi. Tämä on tärkeää ohjelmoinnissa, jotta voidaan esimerkiksi järjestää tapahtumia tai tarkistaa onko tietty päivämäärä menneisyydessä vai tulevaisuudessa.

## Miten?

```C
#include <stdio.h>
#include <time.h>

// esimerkki vertailusta
int compare_dates(struct tm date1, struct tm date2)
{
    if (date1.tm_year < date2.tm_year) return -1;
    else if (date1.tm_year > date2.tm_year) return 1;
    else if (date1.tm_mon < date2.tm_mon) return -1;
    else if (date1.tm_mon > date2.tm_mon) return 1;
    else if (date1.tm_mday < date2.tm_mday) return -1;
    else if (date1.tm_mday > date2.tm_mday) return 1;
    else return 0;
}

int main()
{
    // alustetaan kaksi päivämäärää käyttäen struct tm -rakennetta
    struct tm date1 = { .tm_year = 2020, .tm_mon = 8, .tm_mday = 15 };
    struct tm date2 = { .tm_year = 2021, .tm_mon = 4, .tm_mday = 1 };

    // vertaillaan päivämääriä ja tulostetaan tulos
    int result = compare_dates(date1, date2);
    if (result == -1) printf("Ensimmäinen päivämäärä on aiempi.");
    else if (result == 1) printf("Toinen päivämäärä on aiempi.");
    else printf("Päivämäärät ovat samat.");

    return 0;
}
```

Tulos: Toinen päivämäärä on aiempi.

## Syvempi sukellus

Päivämäärien vertaaminen on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Aiemmin se oli monimutkaisempaa, mutta nykyään C-ohjelmoijat voivat käyttää valmiita funktioita, kuten `difftime()`, joka laskee kahden aikaleiman välisen sekuntimäärän.

Toinen tapa vertailla päivämääriä on käyttää julian päivämääriä, jotka ovat yhtenäisessä muodossa kaikille päivämäärille.

## Katso myös

[Tietoisku C:stä](https://fi.wikipedia.org/wiki/C_(ohjelmointikieli)) - Lisätietoja C-ohjelmoinnista

[Time.h C-dokumentaatio](https://www.tutorialspoint.com/c_standard_library/time_h.htm) - Time.h-kirjaston dokumentaatio, joka sisältää mm. päivämäärän vertailuun tarvittavat funktiot.
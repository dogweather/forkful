---
title:                "Päivämäärän hakeminen"
html_title:           "C: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & miksi?

Nykyisen päivämäärän saaminen on ohjelmoinnissa tavanomainen tehtävä, joka mahdollistaa käyttäjien näkemisen järjestelmissä ja yhteyksissä. Tätä tehdään usein, jotta voidaan tarkistaa, onko jokin toiminto päivitetty viimeisimmästä istunnosta.

## Miten:

Käyttämällä C-ohjelmointikielen date.h ja time.h kirjastoja, voimme helposti saada nykyisen ajan ja päivämäärän. Seuraavassa on esimerkki:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t t = time(NULL);
    struct tm *timeinfo = localtime(&t);
    printf("Nykyinen päivämäärä ja aika: %s\n", asctime(timeinfo));
    return 0;
}
```

Tulostus olisi jotain tällaista:

```
Nykyinen päivämäärä ja aika: Sat Mar 27 14:25:00 2021
```

## Syvällinen sukellus:

Päiväyksen ja ajan saaminen järjestelmissä on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Alun perin se tehtiin käyttämällä POSIX:ää tai ANSI C -toimintoja, mutta nykyään useimmissa kielissä on sisäänrakennettu tuki päivämäärän ja ajan käsittelyyn.

Vaihtoehtoinen tapa saada nykyinen aika ja päivämäärä on käyttää date-komentoa, jolla on useita eri vaihtoehtoja saatavilla. Myös erilaisten laajennettujen kirjastojen, kuten Boost C++ ja Python-kielen datetime-moduulin avulla voi helposti käsitellä päivämääriä ja aikoja monimutkaisemmin.

Tämän kirjoituksen esimerkki käyttää localtime-funktiota, joka muuntaa UTC-aikavyöhykkeen tämänhetkisen ajan paikalliseksi ajaksi. Tämä voi kuitenkin vaihdella käyttöjärjestelmän ja ympäristön mukaan.

## Katso myös:

- [C time.h dokumentointi](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Boost C++ DateTime Library](https://theboostcpplibraries.com/boost.datetime)
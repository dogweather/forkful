---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Päivämäärien vertailu tarkoittaa kahta tai useampaa päivämäärää verrattuna toisiinsa - selvittää, mikä on aikaisempi tai myöhäisempi. Ohjelmoijat tekevät niin laskemaan aikaväliä tai järjestelemään tapahtumia.

## Miten:

C:n standardikirjastosta löytyy `time.h`, jota voit käyttää päivämäärien vertailuun. Seuraava koodi näyttää, miten tämä tehdään.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0}, date2 = {0};
    double seconds;

    date1.tm_year = 2020 - 1900;
    date1.tm_mon = 12 - 1;
    date1.tm_mday = 31;

    date2.tm_year = 2021 - 1900;
    date2.tm_mon = 1 - 1;
    date2.tm_mday = 1;

    seconds = difftime(mktime(&date2), mktime(&date1));

    printf("%.f", seconds);

    return 0;
}
```

Tämän koodin avulla voit nähdä eroavaisuuden päivämäärissä 2020-12-31 ja 2021-01-01 sekunneissa.

## Syvällistä Tietoa

Historiallisessa kontekstissa, päivämäärien vertailu on ollut ja on edelleen ydinkysymys ajan hallinnassa. Käytännön toteuttamisessa käytämme gregoriaanista kalenteria, johon `time.h` perustuu.

Olisi mahdollista tehdä päivämäärien vertailu manuaalisesti - erottamalla ja vertaamalla päivä-, kuukausi- ja vuosikomponentit. Tämä olisi kuitenkin huomattavasti monimutkaisempi prosessi ottaen huomioon kuukausien erilaisen päivien määrän ja karkausvuodet.

`time.h` -kirjasto käyttää `tm` -rakennetta päivämäärien säilyttämiseen. Sen jäsenet - `tm_year`, `tm_mon` ja `tm_mday` säilyttävät vuodet (vuodesta 1900 lähtien), kuukaudet (0-11) ja päivät (1-31), minkä avulla päivämäärät voidaan määrittää ja verrata kätevästi.

## Katso Myös:

1. Julkinen [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm) -sivusto, jossa selitetään `time.h`-kirjaston käyttöä.
2. C-kielen manuaalissa on erittäin yksityiskohtainen kohta päivämäärien ja ajan käsittelystä: [C library to handle dates and times](https://www.gnu.org/software/libc/manual/html_mono/libc.html#Date-and-Time).
3. Laaja lisätietokatsaus päivämäärien manipulointiin ohjelmoinnissa: [Wikipedia's article on Date and Time Notation](https://en.wikipedia.org/wiki/Date_and_time_notation_by_country).
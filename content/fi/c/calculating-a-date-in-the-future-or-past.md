---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "C: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Lasketaan tulevaisuuden tai menneisyyden päivämäärä tietokoneohjelmoinnissa, kun pitää ottaa huomioon tapahtumat, jotka tapahtuvat muina aikoina kuin nykyhetkessä. Tämä on hyödyllistä päivämäärän erityisominaisuuksien simuloinnissa, esimerkiksi lomakekäsittelyssä tai tapahtumien ajoituksessa.

## Näin se tehdään:
C-kielessä voidaan laskea tuleva tai menneisyyden päivämäärä käyttämällä `time.h` -kirjastoa.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm newdate;
    double seconds;

    time(&now);
    newdate = *localtime(&now);

    /* lisätään päiviä */
    newdate.tm_mday += 5;

    /* muutetaan takaisin sekunneiksi */
    seconds = difftime(mktime(&newdate),now);

    printf("%.f seconds from now...\n", seconds);

    return 0;
}
```
## Syvempi sukellus
Laskennan päivämäärät ovat olleet ohjelmoinnin ydinosa jo alkuajoista lähtien. Tämä johtuu siitä, että monet sovellukset, kuten laskut, palkanlaskenta, ja tapahtumien seuranta, vaativat ajan käsittelyä.

Vaihtoehtona C:n `time.h` -kirjastolle, voit käyttää muita päivämäärän ja ajan käsittelyn kirjastoja, kuten Boost.Date_Time tai ICU.

C:n `time.h`-kirjaston `tm`-rakenteen `tm_mday` -kenttä edustaa kuukauden päivää. Tätä voidaan käyttää päivien lisäämiseen tai vähentämiseen. Tämän vaiheen jälkeen `mktime` -funktiota käytetään muuttamaan uusi päivämäärä takaisin sekunneiksi.

## Katso myös
1. Manuaali "time.h" C-kielen standardikirjastosta: [link](http://www.cplusplus.com/reference/ctime/)
2. Boost.Date_Time-kirjasto: [link](https://www.boost.org/doc/libs/1_46_1/doc/html/date_time.html)
3. Päivämäärän ja ajan käsittelyn kansainvälinen komponentti (ICU): [link](https://unicode-org.github.io/icu/userguide/datetime/calendar/)
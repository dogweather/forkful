---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:38.482040-07:00
description: "P\xE4iv\xE4m\xE4\xE4rien vertailu C-kieless\xE4 tarkoittaa niiden kronologisen\
  \ suhteen m\xE4\xE4ritt\xE4mist\xE4 - onko toinen p\xE4iv\xE4m\xE4\xE4r\xE4 ennen\
  \ toista tai ovatko ne samat. T\xE4m\xE4 kyky\u2026"
lastmod: '2024-03-13T22:44:57.053904-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4rien vertailu C-kieless\xE4 tarkoittaa niiden kronologisen\
  \ suhteen m\xE4\xE4ritt\xE4mist\xE4 - onko toinen p\xE4iv\xE4m\xE4\xE4r\xE4 ennen\
  \ toista tai ovatko ne samat. T\xE4m\xE4 kyky\u2026"
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertailu"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärien vertailu C-kielessä tarkoittaa niiden kronologisen suhteen määrittämistä - onko toinen päivämäärä ennen toista tai ovatko ne samat. Tämä kyky on olennainen sovelluksissa, jotka käsittelevät aikataulutusta, määräaikoja tai kirjanpitoa, koska se mahdollistaa aikaan sidotun tiedon järjestämisen ja manipuloinnin.

## Miten:

C ei sisällä valmista tyyppiä päivämäärille, mikä tekee `time.h` kirjaston käytön tarpeelliseksi työskentelyssä päivämäärä- ja aikarakenteiden kanssa. `tm` rakenne ja `difftime()` funktio ovat yleisesti käytössä päivämäärien vertailussa. Alla on esimerkki, joka näyttää, miten vertailla kahta päivämäärää:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Ensimmäinen päivämäärä (VVVV, KK, PP)
    date1.tm_year = 2023 - 1900; // Vuosi vuodesta 1900
    date1.tm_mon = 3 - 1;        // Kuukausi [0-11]
    date1.tm_mday = 15;          // Kuukauden päivä [1-31]

    // Toinen päivämäärä (VVVV, KK, PP)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Muunna time_t formaattiin
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Vertaa
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("Päivämäärät ovat samat.\n");
    } else if (seconds > 0) {
        printf("Ensimmäinen päivämäärä on toisen jälkeen.\n");
    } else {
        printf("Ensimmäinen päivämäärä on ennen toista.\n");
    }

    return 0;
}
```

Tuloste voi olla:

```text
Ensimmäinen päivämäärä on ennen toista.
```

Tämä ohjelma alustaa kaksi `tm` rakennetta tietyillä päivämäärillä, muuntaa ne `time_t` formaattiin käyttämällä `mktime()`, ja lopuksi vertaa niitä käyttämällä `difftime()`, joka palauttaa aikaeron sekunteina (kaksinkertaisena) kahden ajankohdan välillä.

## Syväsukellus

C:n alkuaikoina päivämäärä- ja aikaoperaatiot vaativat manuaalisia laskelmia, ottaen usein huomioon karkausvuodet, kuukausien vaihtelevat päivämäärät ja jopa karkaussekunnit. `Time.h` esittely ANSI C -standardissa toi standardisoinnin C:n ajan käsittelyyn, yksinkertaistaen päivämäärä- ja aikaoperaatioita.

`Time.h` käyttö päivämäärien vertailussa on suoraviivaista, mutta sillä on rajoituksensa. `Tm` rakenne ei ota huomioon aikavyöhykkeitä tai kesäaikaa, ja `difftime()` tarjoaa vain eron sekunteina, puuttuen hienojakoisempaa tarkkuutta tietyissä sovelluksissa.

Sovelluksille, jotka vaativat kestävämpiä päivämäärä-aika operaatioita, mukaan lukien tuki aikavyöhykkeille, kesäaikasiirtymille ja tarkemmille aikaväleille, kirjastot kuten `date.h` (Howard Hinnantin päivämääräkirjasto, ei osa standardikirjastoa) tarjoavat modernin vaihtoehdon `time.h`:lle. Nämä kirjastot tarjoavat kattavampia työkaluja päivämäärä-ajan manipulointiin C++:ssa, hyötyen vuosikymmenten kehityksestä ohjelmointikielisuunnittelussa. C-ohjelmoijille jää edelleen tarve käyttää näitä ulkopuolisia kirjastoja tai huolellisesti käsitellä päivämäärä-ajan laskentanäkökohtia suoraan saavuttaakseen tarkan ja kulttuurisesti tietoisen päivämäärä-ajan manipuloinnin.

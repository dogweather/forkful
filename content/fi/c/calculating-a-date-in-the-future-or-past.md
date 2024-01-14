---
title:                "C: Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Tässä blogikirjoituksessa käsittelemme miten voit laskea tulevaisuuden tai menneisyyden päivämääriä käyttäen C-ohjelmointikieltä. Tämän taidon avulla voit helposti tehdä aikaperusteisia laskelmia esimerkiksi laskutusohjelmissa tai tapahtuma-aikatauluissa.

## Kuinka teet sen

Tämän taidon oppiminen vaatii vain muutaman yksinkertaisen C-komentosarjan. Ensinnäkin, sinun täytyy sisällyttää <time.h> -kirjasto ohjelmaasi, jotta voit käyttää aikaan liittyviä funktioita. Tämän jälkeen voit käyttää time_t-tyyppisiä muuttujia tallentaaksesi päivämäärät.

```C
#include <time.h>
...
time_t tuleva_aika;
time(&tuleva_aika);
//tämä tallentaa tulevan ajan muuttujaan
```

Seuraavaksi käytät mktime-funktiota laskeaksesi haluamasi päivämäärän. Tämä funktio ottaa parametreiksi vuoden, kuukauden, päivän ja tunnin arvot ja palauttaa time_t-tyypin arvon.

```C
struct tm aika;
aika.tm_year = 2021 - 1900; //vuosi - 1900
aika.tm_mon = 9; //kuukausi, tammikuu on 0 ja joulukuu 11
aika.tm_mday = 20; //päivä
aika.tm_hour = 12; //tunti
time_t tuleva_aika = mktime(&aika); //laskee ajan ja tallentaa sen muuttujaan
```

Lopuksi voit tulostaa haluamasi päivämäärän käyttäen ctime-funktiota, joka ottaa time_t-tyypin muuttujan parametrina ja palauttaa merkkijonon muodossa "päivämäärä ja kellonaika".

```C
printf("Tuleva aika: %s\n", ctime(&tuleva_aika));
//tulostaa: Tuleva aika: Wed Oct 20 12:00:00 2021
```

## Syvemmälle

Kaikkien aikaan liittyvien funktioiden käyttöohjeet löytyvät <time.h> -kirjaston dokumentaatiosta. Voit myös muuttaa mktime-funktion palauttaman time_t-tyypin arvon haluamasi päivämäärän lisäämällä tai vähentämällä sen arvoja. Esimerkiksi:

```C
tuleva_aika += 24 * 60 * 60; //lisää yhden päivän aikaan
```

Muista myös, että time_t-muuttuja tallentaa ajan Unix-timestamp-muodossa, jossa aikayksikkönä käytetään sekunteja. Tämä voi olla hyödyllistä, jos haluat tehdä tarkempia aikalaskelmia.

## Katso myös

- [C-kielen <time.h> kirjaston dokumentaatio](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Lyhyt opas Unix-timestampeista](https://www.epochconverter.com/)
- [Ohjelmointiopas aloittelijoille C-kielellä](https://www.valopaa.fi/c-opas-aloittelijoille/)
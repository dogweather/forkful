---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
date:                  2024-01-20T17:28:33.477150-07:00
model:                 gpt-4-1106-preview
html_title:           "C#: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä – toisinaan tarvitaan päivää esim. eräpäivien tai tapahtumien suunnittelussa. Ohjelmoijat tekevät tämän automatisoidakseen prosesseja ja välttääkseen virheitä manuaalisessa laskennassa.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>
#include <time.h>

void add_days(struct tm *date, int days) {
    const time_t ONE_DAY = 24 * 60 * 60;
    time_t date_seconds = mktime(date) + (days * ONE_DAY);
    *date = *localtime(&date_seconds);
}

int main() {
    struct tm future_date = {0};
    time_t rawtime;
    time(&rawtime);
    future_date = *localtime(&rawtime);
    
    add_days(&future_date, 10); // Lisää 10 päivää nykyiseen päivään

    printf("10 päivän päästä on: %02d-%02d-%d\n",
           future_date.tm_mday,
           future_date.tm_mon + 1, // tm_mon on 0-indisoitu
           future_date.tm_year + 1900); // tm_year on vuodesta 1900

    return 0;
}
```

Sample Output:
```
10 päivän päästä on: 25-03-2023
```

## Deep Dive (Syväsukellus):
Aika- ja päivämäärälaskelmat C-kielessä ovat olleet aina taitolaji. Ajanlaskennan historiasta tiedämme mm. Y2K-bugin. C:ssä `time.h`-kirjaston `tm`-rakenne yhdessä `mktime` ja `localtime` funktioiden kanssa tarjoaa perustyökalut oman päivämäärälaskurin toteutukseen. Eri aikavyöhykkeet ja karkausvuodet tuovat lisähaasteita. Vaihtoehtoisia kirjastoja, jotka hoitavat monimutkaisemmatkin laskelmat, löytyy, esim. `DateTime`.

## See Also (Katso Myös):
- ISO C standardi: https://www.iso.org/standard/74528.html
- Tietoa time.h-kirjastosta: https://en.cppreference.com/w/c/chrono
- Aikamuunnokset ja -laskelmat: https://www.epochconverter.com/programming/c
- Advanced date/time libraries (DateTime): https://github.com/HowardHinnant/date

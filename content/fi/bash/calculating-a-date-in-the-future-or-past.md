---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Bash: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lasketaanpa päivämäärä tulevaisuudessa tai menneisyydessä. Tämä on usein tarpeen, kun ohjelmoijat haluavat suunnitella tai logata tapahtumia suhteessa nykyhetkeen.

## Kuinka:

```Bash
#Lasketaan 7 päivää tulevaisuudessa
date -d "+7 days"

#Lasketaan 30 päivää menneisyydessä
date -d "-30 days"
```

Tämä toimii, koska `date`-komento Linuxissa tulkitaan -d (eli --date) vaihtoehdon avulla syötteenä olevan päivämäärän.

## Syväsukellus:

`date`-komento tuli alunperin Unix-järjestelmästä, ja nyt se on laajalti saatavilla useimmissa järjestelmissä. Vaikka `date` on hyödyllinen, on olemassa muita työkaluja, kuten `at` ja `cron`, joiden avulla voit suunnitella komentojen suorittamista tietyissä ajankohdissa.

Linuxin `date`-komennot käyttävät GNU coreutils -pakettia, joka on moniulotteinen työkalusarja tälle alustalle. Se ymmärtää monenlaisia päivämäärän syöttömuotoja, esimerkiksi "+1 month 2 days" tai "-3 weeks".

## Katso myös:

- GNU coreutils: [www.gnu.org/software/coreutils/coreutils.html](https://www.gnu.org/software/coreutils/coreutils.html)
- `date` man sivu: [man7.org/linux/man-pages/man1/date.1.html](http://man7.org/linux/man-pages/man1/date.1.html)
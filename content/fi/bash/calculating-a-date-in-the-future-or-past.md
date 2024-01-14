---
title:                "Bash: Päivän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Tässä blogipostauksessa selitämme, miksi olisi hyödyllistä oppia laskemaan päivämääriä tulevaisuudessa tai menneisyydessä.

## Miten

Voit helposti laskea tiettyä päivämäärää tulevaisuudessa tai menneisyydessä käyttämällä Bash-ohjelmointikieltä. Alla olevat koodilohkot näyttävät esimerkin tulevasta ja menneestä päivästä.

```Bash
# Laske päivämäärä kaksi viikkoa tulevaisuudessa
date -d "+2 weeks"

Tulostus: Fri May 28 00:00:00 EEST 2021

# Laske päivämäärä yksi vuosi ja kuusi kuukautta menneisyydessä
date -d "-1 year -6 months"

Tulostus: Sun Nov 29 00:00:00 EET 2019
```

Voit myös asettaa tietyn päivämäärän, josta haluat laskea eteen- tai taaksepäin.

```Bash
# Laske päivämäärä viisi päivää tulevaisuudessa
date -d "2021-04-25 +5 days"

Tulostus: Fri Apr 30 00:00:00 EEST 2021

# Laske päivämäärä kolme kuukautta ja kaksi viikkoa menneisyydessä
date -d "2019-08-10 -3 months -2 weeks"

Tulostus: Wed Apr 24 00:00:00 EEST 2019
```

## Syvällinen sukellus

Bashin `date` -komento hyödyntää ISO 8601-standardin mukaista päivämäärä- ja aikamuotoilua. Voit lukea lisää siitä [täältä](https://en.wikipedia.org/wiki/ISO_8601). Lisäksi voit tarkastella `man date` -komentoa saadaksesi lisätietoja eri vaihtoehdoista, joita voit käyttää päivämäärien laskemiseen.

Samalla tavalla voit myös käyttää muita Bashin sisäänrakennettuja funktioita, kuten `grep`, `sed` ja `awk`, muokkaamaan tiettyjä päivämääriä ja tulostamaan haluamasi formaatin.

## Katso myös

- [Date manuaalisivu](https://man7.org/linux/man-pages/man1/date.1.html)
- [ISO 8601-standardi](https://en.wikipedia.org/wiki/ISO_8601)
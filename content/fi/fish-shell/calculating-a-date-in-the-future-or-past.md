---
title:                "Fish Shell: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi laskisin tulevan tai menneen päivän Fish Shell ohjelmoinnilla? Päivämäärän laskeminen voi olla hyödyllistä esimerkiksi laskuissa, tapahtumien suunnittelussa tai muissa sovelluksissa, jotka tarvitsevat tietoa ajasta.

## Miten

Fish Shell tarjoaa helpon tavan laskea tulevan tai menneen päivän käyttämällä `date` komentoa. Tässä esimerkissä lasketaan päivämäärä 7 päivää tulevaisuuteen ja tulostetaan se konsoliin:

```Fish Shell
date -d "+7 days"
```

Tulostus:

```
Fri Jan 10 17:41:10 EET 2020
```

Samalla tavalla voit laskea myös menneen päivän antamalla negatiivisen päivien määrän. Esimerkiksi, jos haluat tietää millainen päivä oli 100 päivää sitten, käytä tätä komentoa:

```Fish Shell
date -d "-100 days"
```

Tulostus:

```
Mon Sep 23 17:41:10 EEST 2019
```

Voit myös asettaa tietyn päivämäärän lähtökohdaksi laskuille antamalla päivämääräyksen muodossa `yyyy-mm-dd`. Esimerkiksi, jos haluat laskea päivämäärän 30 päivää 1.1.2020 jälkeen, käytä tätä komentoa:

```Fish Shell
date -d "2020-01-01 +30 days"
```

Tulostus:

```
Sat Feb 01 17:41:10 EET 2020
```

## Syvemmälle

Fish Shellin `date` komento tukee myös muita vaihtoehtoja, kuten eri aikavyöhykkeiden ja kelloaikojen asettamista. Voit tutustua näihin vaihtoehtoihin käyttämällä komentoa `man date`.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/cmds/date.html)
- [30 päivää koodausta - Fish Shellin perusteet](https://30paivaa.koodiaapinen.fi/fish-shellin-perusteet/)
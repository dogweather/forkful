---
title:    "Fish Shell: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen laskea tuleva tai mennevä päivämäärä tietyn ajanjakson päästä tai eteenpäin. Tässä blogipostissa opimme kuinka voimme käyttää Fish Shellia helpottamaan tätä tehtävää. 

## Kuinka Tee

Käytämme Fish Shellin `date` komentoa laskeaksemme päivämäärän halutusta ajankohdasta tulevaisuudessa tai menneisyydessä. Esimerkiksi, jos haluamme laskea päivämäärän 10 päivän päästä tästä päivästä, kirjoitamme seuraavanlaisen komennon:

```Fish Shell
date -d '+10 days'
```

Tämä tulostaa päivämäärän muodossa `ma touko 03 2021`. Voimme myös käyttää muita aikayksiköitä, kuten viikkoja, kuukausia tai vuosia lisäämään tai vähentämään päivämäärää.

Voimme myös määrittää tietyn päivämäärän ja laskea sitten päivien eron tuon päivämäärän ja nykyisen päivämäärän välillä. Esimerkiksi, jos haluamme tietää kuinka monta päivää on jäljellä seuraavaan syntymäpäiväämme, kirjoitamme seuraavanlaisen komennon:

```Fish Shell
date -d '2022-05-01 - now'
```

Tämä tulostaa päivien lukumäärän muodossa `334 days`.

## Syvällinen Sukellus

Fish Shellin `date` komento tarjoaa monia vaihtoehtoja päivämäärän laskemiseen tulevaisuudessa tai menneisyydessä. Voimme esimerkiksi käyttää `-r` vaihtoehtoa nähdäksemme päivämäärän tietylle ajankohdalle unix timestampina. Voimme myös käyttää `-s` vaihtoehtoa asettaaksemme halutun päivämäärän tietylle ajankohdalle.

Voit löytää lisätietoa Fish Shellin `date` komennosta kirjoittamalla komentoriville `man date`.

## Katso myös

- [Fish Shell Dokumentaatio - Date Komennon Käyttö] (https://fishshell.com/docs/current/cmds/date.html)
- [Linux.fi - Aikaleimojen Käyttö] (https://linux.fi/wiki/Aikaleima)
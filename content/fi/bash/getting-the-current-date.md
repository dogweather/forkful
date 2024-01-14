---
title:                "Bash: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi Päivämäärää Kannattaa Hakea

Haluatko lisätä päivämäärämerkinnän skriptiisi? Ehkä haluat luoda tiedoston, jossa sen luomispäivä on määritetty automaattisesti? Tai ehkä haluat vain tietää tarkalleen, milloin tietty komento suoritettiin. Riippumatta syystä, Bashilla on helppo tapa saada nykyinen päivämäärä ja aika.

## Kuinka Hakea Nykyinen Päivämäärä

On olemassa useita tapoja saada nykyinen päivämäärä Bashissa. Ensimmäinen ja yksinkertaisin tapa on käyttää komentoa `date` ja sen erilaisia vaihtoehtoja.

```Bash
date
```

Tämä tulostaa nykyisen päivämäärän ja ajan, muodossa `Mon Jan 18 11:46:32 UTC 2021`.

Jos haluat päivämäärän tietyssä muodossa, voit käyttää `date` komentoa ja määrittää sen formaatin `+` -merkillä.

```Bash
date +"%d.%m.%Y"
```

Tämä tulostaa nykyisen päivämäärän muodossa `18.01.2021`.

Voit myös käyttää `date` komentoa erilaisilla vaihtoehdoilla saadaksesi tiettyjä tietoja, kuten vain päivämäärä tai aika.

```Bash
date +"%A" # Tulostaa nykyisen päivän, esimerkiksi "Maanantai"
date +"%R" # Tulostaa nykyisen ajan formaatissa HH:MM, esimerkiksi "11:46"
```

## Syvempi Sukellus

`date` komennolla on monia vaihtoehtoja, joita voit käyttää saadaksesi erilaisia tietoja nykyisestä päivämäärästä ja ajasta. Voit tutustua kaikkiin saatavilla oleviin vaihtoehtoihin Bashin manuaalisivulla `man date`.

Voit myös käyttää `date` komentoa muuttujien määrittämiseen ja tallentamaan päivämäärän tai ajan johonkin muuhun komentoon tai skriptiin.

```Bash
CURRENT_DATE=$(date +"%m-%d-%Y")
echo "Tämän päivän päivämäärä on $CURRENT_DATE"
```

Tämä tallentaa muuttujaan `CURRENT_DATE` nykyisen päivämäärän muodossa `01-18-2021` ja tulostaa sen `echo` komennolla.

## Katso Myös

- [Bashin manuaalisivu](https://www.gnu.org/software/bash/manual/bash.html)
- [Bashin muuttujat](https://www.pluralsight.com/blog/it-ops/linux-basics-when-to-use-environment-variables-vs-shell-variables)
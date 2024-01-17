---
title:                "Päivämäärän hankkiminen"
html_title:           "Bash: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän hakeminen tarkoittaa nykyisen päivämäärän ja kellonajan tarkistamista. Ohjelmoijat tekevät tämän yleensä siksi, että he haluavat tallentaa tietyn tapahtuman aikaleiman tai suorittaa erilaisia ajanhallintatoimintoja ohjelmassaan.

## Miten:

```Bash
# Näytä nykyinen päivämäärä ja kellonaika
date

# Tallenna nykyinen päivämäärä ja kellonaika muuttujaan
current_date=$(date)

# Näytä nykyinen päivämäärä ja kellonaika määritellyssä muodossa
date +"%d/%m/%Y %H:%M:%S"
```

Esimerkkitulosteet:

```
ke toukokuu 19 14:32:24 EEST 2021
19/05/2021 14:32:24
```

## Syvemmälle:

Hakemisen päivämäärä on ollut tärkeä osa ohjelmointia jo pitkään ja se on yleensänistetty käyttöjärjestelmän tehtäväksi. Bashilla on kuitenkin oma sisäänrakennettu date-komento, joka helpottaa päivämäärän hakemista ohjelmassa.

Toinen tapa saada nykyinen päivämäärä on käyttää komentoa ```echo $(date)```, joka käyttää Shell Expansion -ominaisuutta palauttaakseen päivämäärän. Tämä voi olla hyödyllistä ohjelmoinnissa, kun päivämäärää käytetään merkkijonona.

## Katso myös:

- [Bash Manual: date](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-date)
- [How to get current date and time in Bash](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Alimerkkijonon purkaminen, tai substrings, on prosessi, jossa irrotetaan osa merkkijonosta. Ohjelmoijat tekevät tämän tiedon hajottamiseksi ja jalostamiseksi helpommin hallittaviksi osiksi.

## Kuinka tehdä:

Viedään läpi muutama esimerkki Fish Shellin kanssa. Oletetaan, että sinulla on seuraava merkkijono: "Tervetuloa Fish Shell -ohjelmointiin". 

```Fish Shell
set str "Tervetuloa Fish Shell -ohjelmointiin"
echo $str[7..15]
```
Tämä tulostaa "Fish Shell", koska se on indeksien 7 ja 15 välissä.

## Syvä sukellus:

Fish Shell, perustettu vuonna 2005, tarjoaa yksinkertaisen tavan käsitellä merkkijonoja. Toiset shellit, kuten Bash tai Zsh, ovat vaihtoehtoja, mutta niiden kanssa joudut tekemään enemmän työtä saadaksesi saman tuloksen.

Fish Shell käsittelee merkkijonon purkamisen hyödyntämällä Slicing-mekanismia, jonka avulla valikoitu segmentti merkkijonosta voidaan poimia.

## Katso myös:

Fish Shell dokumentaatio: https://fishshell.com/docs/current/
Merkkijonojen käsittelyn perusteet: https://www.learnshell.org/
Tietoa Slicing-mekanismista: https://en.wikipedia.org/wiki/Array_slicing
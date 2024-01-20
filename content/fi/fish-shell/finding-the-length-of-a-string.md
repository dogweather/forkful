---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen pituuden selvittäminen tarkoittaa sen selvittämistä, kuinka monta merkkiä merkkijonossa on. Ohjelmoijat tekevät tämän usein, kun haluavat tarkistaa, onko merkkijono tyhjä, tai kun heidän on säädettävä merkkijonoja tietyllä tavalla.

## Näin se tehdään:

Fish Shellissa merkkijonon pituuden saa selville käyttämällä `string length` -toimintoa.

```fish
# Esimerkki 1 - merkkijonon "Hei maailma" pituus
set lause "Hei maailma"
echo (string length $lause)
```

Tässä esimerkissä tulostuu `11`, koska merkkijono "Hei maailma" on 11 merkkiä pitkä.

## Syvemmälle:

Merkkijonojen pituuden selvittäminen on yksinkertainen mutta keskeinen toiminto suurimmassa osassa ohjelmointikieliä. Fish Shell tarjoaa yksinkertaisen ja suoraviivaisen tavan selvittää merkkijonon pituus `string length` toiminnon avulla.

Vaihtoehtoisesti voit käyttää `wc`-komentoa (word count), joka laskee myös rivit ja sanat sekä merkit.

```fish
echo -n $lause | wc -m
```

Tässä `wc -m` laskee merkit, ja `-n` estää rivinvaihdon tulostumisen.

Merkkijonojen pituuden selvittämiseen liittyy yksityiskohtia, kuten se, että tietyt merkit, kuten välilyönnit, lasketaan mukaan pituuteen. Tämä on tärkeää muistaa, kun manipuloidaan merkkijonoja.

## Katso myös:

1. Fish Shell dokumentaatio - [Merkkijonot](https://fishshell.com/docs/current/commands.html#string)
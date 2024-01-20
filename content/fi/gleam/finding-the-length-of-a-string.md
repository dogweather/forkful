---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen pituuden löytäminen kertoo sinulle kuinka monta merkkiä tietyssä merkkijonossa on. Ohjelmoijat tekevät tämän lukemattomista syistä - esimerkiksi tulostusmuotoiluun, syntaksin tarkastukseen tai merkkijonon osien hallintaan.

## Kuinka Tehdä:

```Gleam
import gleam/unicode

var message = "Hei maailma"
unicode.length(message) // Tämä palauttaa 11
```

Edellisessä esimerkissä "Hei maailma" -merkkijono koostuu 11 merkistä, jotka lasketaan käyttämällä `unicode.length(message)` funktiota.

## Deep Dive:

Menettely merkkijonojen pituuden löytämiseksi on melko vanha ja yksinkertainen, mutta se on yleisesti hyväksytty ohjelmointitekniikka. Gleam toteuttaa sen `unicode.length/1` -funktioon, joka laskee merkkien lukumäärän Unicode-muodossa. 

Vaihtoehtoinen tekniikka Gleamissa on laskeminen manuaalisesti, mutta se on yleensä raskasta ja tehotonta. Kuitenkin, se voi antaa kohdennetumpaa hallintaa erityisolosuhteissa.

Merkkijonon pituuden saaminen Gleamissa voi vaikuttaa yksinkertaiselta, mutta sen takana on melko monimutkainen algoritmi. Gleam käyttää Unicode-standardia merkkijonojen esittämiseen. Tämä tarkoittaa, että jokainen merkki voi koostua yhdestä tai useammasta tavusta. Tästä johtuen `unicode.length/1` on epätriviaalinen operaatio, joka tarkistaa jokaisen merkin perusteellisesti.

## Katso Myös:

Parhaita lähteitä merkkijonojen pituuden löytämiseksi Gleamissa ovat:

2. Stack Overflow -keskustelut aiheesta: [Finding String Length in Gleam](https://stackoverflow.com/questions/tagged/gleam?tab=Votes)
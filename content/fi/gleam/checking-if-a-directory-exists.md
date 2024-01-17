---
title:                "Tarkistaakko hakemisto on olemassa"
html_title:           "Gleam: Tarkistaakko hakemisto on olemassa"
simple_title:         "Tarkistaakko hakemisto on olemassa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistaa onko hakemisto olemassa. Tämä on tärkeä askel monissa ohjelmoinnin projekteissa, koska luoja määritelmän täytyy tietää onko hakemisto olemassa tai ei.

## Miten:

```Gleam
let exists = os.fs.exists("hakemisto")
```

Tässä yksinkertaisessa esimerkissä käytämme Gleamin `os.fs` -pakettia tarkistamaan, onko hakemisto nimeltään "hakemisto" olemassa. Tulos tallennetaan muuttujaan `exists`, joka on joko `true` tai `false`.

## Syvemmälle:

Hakemistojen olemassaolon tarkistaminen on tullut tärkeäksi nykyaikaisissa ohjelmoinnin projekteissa, joissa tarvitaan tiedostojen ja hakemistojen käsittelyä. Se auttaa varmistamaan, että halutut tiedostorakenteet ovat olemassa ennen kuin niihin yritetään viitata.

Vaihtoehtoisesti hakemiston olemassaolon voi tarkistaa myös käyttämällä operatiivijärjestelmän komentoja, kuten `ls` tai `dir`. Gleam tarjoaa kuitenkin yksinkertaisen ja suoraviivaisen tavan tehdä tämä suoraan ohjelmassa.

Tämän toteuttamiseksi Gleam käyttää operatiivijärjestelmän `stat` -komentoa, joka palauttaa tiedon tiedostosta tai hakemistosta. Tämän tiedon avulla Gleam voi tarkistaa onko hakemisto olemassa vai ei.

## Katso myös:

- [Gleamin virallinen dokumentaatio](https://gleam.run/)
- [Gleam `os.fs` -paketin dokumentaatio](https://gleam.run/news/0.15.0-released.html)
- [Operatiivijärjestelmän `stat` -komennon dokumentaatio](https://en.wikipedia.org/wiki/Stat_(system_call))
---
title:                "Gleam: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Monet meistä, jotka oppivat koodaamaan, alkavat tavallisesti tekemään pieniä projekteja, kuten luomaan yksinkertaisia ohjelmia tai verkkosivuja. Yksinkertainen tekstieditorin käyttö on tässäkin vaiheessa tärkeää, sillä se auttaa meitä ymmärtämään, kuinka tiedostot tallennetaan ja miten koodi vaikuttaa tiedoston sisältöön.

## Kuinka tehdä

Gleam-ohjelmointikielen avulla voit helposti luoda ja tallentaa tekstitiedostoja. Alla on esimerkki koodista, joka luo uuden tekstitiedoston nimeltä "esimerkki.txt" ja tallentaa siihen tekstin "Hei Maailma!".

```Gleam
kekkeri = "Hei Maailma!"

gleam:file.write("esimerkki.txt", kekkeri)
```

Ajaessamme tätä koodia, se luo uuden tekstitiedoston kanssa nimen "esimerkki.txt" nykyiseen työskentelyhakemistoon ja kirjoittaa sen sisältöksi "Hei Maailma!".

## Syvällinen sukellus

Tekstitiedostojen luominen ja tallentaminen on tärkeä taito jokaiselle Gleam-ohjelmoijalle. On tärkeää muistaa, että tekstitiedostoilla voi olla erilaisia muotoiluja, kuten HTML tai CSV, ja Gleamilla on erityisiä kirjastoja näiden luomiseen ja käsittelyyn. Myös tekstitiedostojen lukeminen ja muokkaaminen on taito, joka on hyödyllinen mille tahansa ohjelmoijalle.

## Katso myös

- [Gleam-ohjelmointikielen virallinen sivusto](https://gleam.run/)
- [Gleamin dokumentaatio tekstitiedoston käsittelyyn](https://gleam.run/documentation/libraries/files/)
- [Esimerkkejä tekstitiedoston käytöstä Gleamilla](https://github.com/gleam-lang/gleam/tree/master/examples/files)
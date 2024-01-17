---
title:                "Tekstin etsiminen ja vaihtaminen"
html_title:           "Gleam: Tekstin etsiminen ja vaihtaminen"
simple_title:         "Tekstin etsiminen ja vaihtaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hakeminen ja tekstin korvaaminen on tärkeä osa ohjelmoinnissa! Se tarkoittaa sitä, että voit etsiä tietyt kohdat tiedostosta ja korvata ne uudella tekstillä. Tämä on erityisen hyödyllistä, kun sinun pitää tehdä samanlaisia muutoksia useissa kohdissa koodissasi.

## Kuinka:

```Gleam
String.replace("Tervetuloa, maa!", "maa", "Gleam")
```

Tämä koodinpätkä korvaa sanan "maa" tekstissä "Tervetuloa, maa!" Gleam-sanalla, ja lopputuloksena on "Tervetuloa, Gleam!".  Voit myös käyttää säännöllisiä lausekkeita hakemiseen ja korvaamiseen:

```Gleam
Regex.replace(~r/banaani/, "Äpple", "Minun suosikkiruoka on banaani")
```

Tässä esimerkissä säännöllinen lauseke etsii sanan "banaani" ja korvaa sen tekstillä "Äpple", jolloin lopputuloksena on "Minun suosikkiruoka on Äpple".

## Syvällinen sukellus:

Hakeminen ja tekstin korvaaminen on ollut käytössä ohjelmistoissa jo pitkään. Aiemmin sitä tehtiin manuaalisesti, mutta nykyään siihen on tarjolla monia eri työkaluja ja ohjelmistoja. Esimerkiksi Unix-järjestelmissä käytetään usein komentoja kuten sed ja awk tekstien muokkaamiseen ja korvaamiseen.

## Katso myös:

Voit lukea lisää Gleamin tekstin korvaamisesta dokumentaatiostamme: [https://gleam.run/documentation/strings/](https://gleam.run/documentation/strings/)

Voit myös tutustua muihin hyödyllisiin ohjelmistojen kirjoittamisen vinkkeihin meidän blogistamme: [https://blog.gleam.run/](https://blog.gleam.run/)
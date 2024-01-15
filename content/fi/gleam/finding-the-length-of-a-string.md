---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Gleam: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus tarvitsemme tietää kuinka monta merkkiä on esimerkiksi käyttäjän syöttämässä tekstissä tai tiedoston nimessä. Tämä on hyödyllistä esimerkiksi tietojen käsittelyssä tai tietokantojen hallinnassa.

## Miten

Käytä `length` funktiota laskeaksesi merkkijonon pituus. Voit tallentaa pituuden muuttujaan ja tulostaa sen. Esimerkki:

```Gleam
let merkkijono = "Tämä on esimerkki"
let pituus = length(merkkijono)
io.println("Merkkijonon pituus on:", pituus)
```

Tämä tulostaa:

```
Merkkijonon pituus on: 19
```

## Tarkempi tarkastelu

Merkkijonon pituuden laskemisessa käytetään tavallisesti silmukkaa, joka käy läpi jokaisen merkin ja lisää laskuria joka kierroksella. Gleamissa voit kuitenkin yksinkertaisesti käyttää valmiiksi määriteltyä `length` funktiota, joka tekee tämän työn puolestasi.

## Katso myös

- [Gleam käsikirja](https://gleam.run/manual/index.html)
- [Merkkijonofunktiot Gleamissa](https://gleam.run/manual/stdlib.html#string-string-functions)
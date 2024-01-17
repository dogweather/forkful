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

## Mikä ja Miksi?

Miksi me ohjelmoijat tarvitsemme selvittää merkkijonon pituuden? Koska se auttaa meitä käsittelemään ja hallitsemaan merkkijonoja tehokkaasti. Merkkijonon pituuden löytäminen tarkoittaa yksinkertaisesti sen merkkien määrän laskemista.

## Kuinka tehdä:

Gleamin avulla merkkijonon pituuden löytäminen on helppoa ja nopeaa. Katso alla olevaa koodiesimerkkiä:

```
Gleam.str.len("Tervetuloa Gleamiin!") // palauttaa 20
Gleam.str.len("Tämä on toinen esimerkki.") // palauttaa 26
```

## Syväsukellus

Ennen Gleamin keksimistä ohjelmoijilla oli erilaisia tapoja löytää merkkijonon pituus, kuten for-silmukoilla tai built-in funktioilla muissa kielissä. Mutta Gleam tekee tästä prosessista helppoa ja yksinkertaista, mikä säästää aikaa ja vaivaa.

On myös muita tapoja löytää merkkijonon pituus, kuten käyttämällä slice-operaattoria tai length propertyä JavaScriptissä. Mutta Gleaminkaan avulla tämä ei vaadi ylimääräistä koodia ja on helposti luettavaa.

## Katso myös:

- [Gleamin virallinen dokumentaatio](https://gleam.run/)
- [Merkkijonon pituuden laskeminen JavaScriptillä](https://www.w3schools.com/js/js_string_length.asp)
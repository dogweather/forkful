---
title:                "Gleam: Merkkijonon kiroitus"
simple_title:         "Merkkijonon kiroitus"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Yksinkertaisesti sanottuna, pientenkin tehtävien tehostaminen on tärkeää ohjelmoinnissa. Isossa koodipohjassa, kuten ohjelmistokehityksessä, jokainen pieni apu voi auttaa parantamaan kokonaisuutta. Onneksi Gleam-ohjelmointikielen avulla pystyt helposti kasvattamaan tehokkuutta kehitystyössäsi.

## Miten

Gleam-ohjelmointikielen avulla pystyt helposti muuttamaan merkkijonon ensimmäisen kirjaimen isoksi. Tämä toiminto on erityisen hyödyllinen, kun käsitellään käyttäjän antamia syötteitä, joissa kirjainkoko saattaa vaihdella.

```
Gleam.say("kirjoita tähän joku sana")
|> Gleam.String.capitalize
```

Tässä yksinkertaisessa esimerkissä käyttäjän antama sana "kirjoita" muuttuu muotoon "Kirjoita", joka on yleisesti hyväksytty tapa kirjoittaa suomen kielessä. 

```
[Output] "Kirjoita"
```

## Syvemmälle

Gleam-ohjelmointikielen "capitalize" -funktio tekee taianomaisen asian taustalla. Se käyttää Unicode Standardin toiminnallisuutta, joka huolehtii kulttuuririippuvaisista merkkien suurennoksista. Tämä tarkoittaa, että funktion avulla pystyt käsittelemään kirjaimia kaikista kielistä oikein ja yhtenäisesti. 

## Katso myös

- [Gleam-ohjelmointikielen dokumentaatio](https://gleam.run/documentation/)
- [Unicode Standard](https://unicode.org/standard/standard.html)
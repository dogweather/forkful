---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on tärkeä osa ohjelmointia, sillä se mahdollistaa useiden tekstien yhdistämisen yhdeksi merkkijonoksi. Tämä on erityisen hyödyllistä esimerkiksi käyttäjän syötteen käsittelyssä ja tietojen tallentamisessa.

## Kuinka Tehtävä

Stringien yhdistäminen on helppoa Gleam-ohjelmointikielessä. Voit käyttää stringien yhdistämiseen plus-merkkiä (+) tai käyttämällä string-moduulista löytyvää "concat" -funktiota.

```Gleam
let etunimi = "Mikko"
let sukunimi = "Mäkinen"

let kokonimi = etunimi + " " + sukunimi
//tulostaa "Mikko Mäkinen"

let kokonimi2 = string.concat(etunimi, " ", sukunimi)
//tulostaa myös "Mikko Mäkinen"
```

## Syvemmälle

Stringien yhdistämisessä on hyvä muistaa, että Gleam-kielessä kaikki on immutablea eli muuttumatonta. Tämä tarkoittaa sitä, että alkuperäisiä stringejä ei muokata, vaan uusi stringi luodaan niiden pohjalta.

Stringien yhdistäminen on myös suorituskyvyltään tehokasta Gleamissa, sillä se käyttää sisäisesti StringBuilder -rakennetta. Tämä tarkoittaa, että useiden stringien yhdistäminen ei aiheuta turhia muistiallokaatioita, mikä voi hidastaa ohjelman suoritusta.

## Katso myös

- [Gleam-kielen dokumentaatio](https://gleam.run/)
- [Opas Gleam-ohjelmointikieleen](https://github.com/gleam-lang/gleam/blob/master/getting-started.md)
- [Tekstien käsittely Gleamissa](https://gleam.run/book/tutorials/strings.html)
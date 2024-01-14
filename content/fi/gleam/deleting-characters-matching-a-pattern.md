---
title:                "Gleam: Mallia vastaavien merkkien poistaminen"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
Oletko joskus törmännyt tilanteeseen, jossa haluaisit poistaa tietyt merkit jostakin tekstistä? Ehkä haluat muokata käyttäjän syöttämää tietoa ennen sen tallentamista tietokantaan tai vain yksinkertaisesti siistiä muuttujan sisältöä. Gleam-ohjelmointikielen avulla tämä on helppoa ja nopeaa!

## Miten tehdä
Voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa Gleam-kielen `Regex.replace`-toiminnolla. Alla on esimerkki, jossa poistetaan kaikki välilyönnit antamastamme merkkijonosta ja tulostetaan muokattu versio.

```Gleam
let lause = "Hei, tämä on esimerkkilause."
let kaava = Regex.new("\\s+", [global: true])
let muokattu_lause = Regex.replace(kaava, lause, "")
```

Tämä tulostaa `Heitäänesimerkkilause.`

Voit myös käyttää `Regex.match?`-toimintoa tarkistaaksesi, vastaako merkkijono haluttua kaavaa. Alla on esimerkkejä, joissa tarkistetaan, onko antamamme merkkijono numero tai kirjain.

```Gleam
let numero = Regex.new("\\d+", [global: true])
let onko_numero = Regex.match?(numero, "1234") // Palauttaa true
let kirjain = Regex.new("[a-z]", [global: true])
let onko_kirjain = Regex.match?(kirjain, "Hei") // Palauttaa true
```

## Syventävä sukellus
Regex-toiminto mahdollistaa monipuolisen tekstien muokkauksen Gleam-ohjelmointikielessä. Voit käyttää myös "lipsautinvastaisia" kuvamerkkejä, kuten`\w` merkitsemään sanallisia merkkejä ja `\D` merkitsemään kaikkia paitsi numeerisia merkkejä.

Voit myös lisätä muuttujan arvon kaavaan käyttämällä `#{muuttuja}`. Se vastaa ohjelmointikielien string-interpoloimista.

```Gleam
let muuttuja = "tekshti"
let muokkaus = Regex.new("#{"muuttuja"}", [global: true])
let muokattu_lause = Regex.replace(muokkaus, "Tämä on #{@muuttuja}.")
```

Tämä tulostaa `Tämä on teksti.`

## Katso myös
- [Gleam-ohjelmointikielen virallinen dokumentaatio](https://gleam.run)
- [Regex-toiminnon tiedot Gleam-kielen standardikirjastossa](https://gleam.run/lib/Regex.html)
---
title:                "Jonojen yhdistäminen"
html_title:           "Elixir: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

Näin yhdistät Elixir-merkkijonoja

## Mitä ja miksi?
Merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi. Tämä on hyödyllistä, sillä se säästää aikaa ja parantaa koodin luettavuutta.

## Kuinka:
```Elixir
string1 = "Tervetuloa"
string2 = "maailman"
string3 = "paras"
string4 = "ohjelmointikieli"
neljännenMerkinjono = string1 <> " " <> string2 <> " " <> string3 <> " " <> string4
IO.puts neljännenMerkinjono
```

Tulostaa: "Tervetuloa maailman paras ohjelmointikieli"

## Syväsukellus:
Merkkijonojen yhdistäminen on tullut osaksi ohjelmointikieliä 1990-luvulta alkaen ja se on yksi perusteellisesti käytetystä ohjelmointipotentiaalista. Vaihtoehtoina on käyttää plusoperaattoria (+), mutta kun kyseessä on suuri määrä merkkijonoja, niin tämä ei ole optimaalinen ratkaisu. Elixir käyttää operaattoria <> yhdistämään merkkijonoja, mikä on tehokas ja helppo tapa tehdä se.

## Katso myös:
- [Elixirin virallinen dokumentaatio](https://elixir-lang.org/)
- [Miksi merkkijonoja ei pitäisi koskaan yhdistää plusoperaattorilla](https://til.hashrocket.com/posts/6c7bdd033d-why-you-should-never-concatenate-strings-with-plus)
- [Oppia lisää merkkijonojen käsittelystä Elixirissä](https://www.learnelixir.tv/p/a-deep-dive-into-working-with-strings-in-elixir)
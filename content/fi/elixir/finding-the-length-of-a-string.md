---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Elixir: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & miksi?
Stringin pituuden löytäminen tarkoittaa yksinkertaisesti merkkien määrän laskemista annetusta merkkijonosta. Tätä taitoa tarvitaan useissa ohjelmointitehtävissä, kuten tietyn merkkijonon etsimisessä tai käsittelyssä.

## Kuinka:
Elixirissä stringin pituuden löytämiseen on useita tapoja. Voit käyttää sisäänrakennettua `String.length` funktiota tai `Enum.count` funktiota yhdessä split-toiminnon kanssa. Esimerkiksi:

```Elixir
stringi = "Tervetuloa Elixirin maailmaan!"
String.length(stringi)
# => 28

toinen_stringi = "Elixir on hauskaa opetella"
Enum.count(String.split(toinen_stringi, " "))
# => 4
```

## Syväsukellus:
Stringin pituuden laskeminen on ollut tärkeä osa ohjelmointia jo pitkään. Aikaisemmin se saattoi olla haastavaa, mutta nykyään kieliemme sisäänrakennetut ominaisuudet tekevät siitä helppoa. Joissakin muissa ohjelmointikielissä, kuten C:ssä, täytyy toteuttaa oma funktio stringin pituuden laskemiseen.

## Katso myös:
- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir School - oppimisresurssi Elixirista](https://elixirschool.com/fi/lessons/basics/string-length/)
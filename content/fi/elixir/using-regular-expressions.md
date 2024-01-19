---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Regular expressions, tai säännölliset lausekkeet, ovat sarja merkkejä, joita käytetään kuvaamaan ja löytämään tekstipattereita. Ohjelmoijat käyttävät niitä näiden kaavojen nopeaan ja tehokkaaseen tunnistamiseen ja käsittelyyn.

## Näin se tapahtuu:

Elixirissa säännöllisten lausekkeiden käyttämisessä on muutama askel. Ensinnäkin, meidän on luotava säännöllinen lauseke käyttämällä Erlangin `:re` moduulia:

```
regex = ~r/[A-Z]/
```

Sitten voimme käyttää `Regex.run/2` -toimintoa etsimään ensimmäisen ottelun:

```elixir
iex> Regex.run(regex, "Hello")
["H"]
```

Tai `Regex.scan/2` toimintoa löytämään kaikki tekstipatterit:

```elixir
iex> Regex.scan(regex, "Hello World")
[["H"], ["W"]]
```

## Syvempi sukellus 

Säännölliset lausekkeet, ovat kehittyneet foorumi softasta UNIX:n grep komentoon. Elixir perii Erlangin tehokkaan säännöllisten lausekkeiden moottorin, joka puolestaan perustuu PCRE:hen, joka on nopea ja erittäin luotettava.

Kuten kaikilla työkaluilla, säännöllisillä lausekkeilla on omat rajoitteensa ja tehokkuuden heikkenemistä. Joskus on parempi harkita vaihtoehtoja, kuten merkkijonoja tai ottelijoita Elixirin sopivuudella.

Säännöllisten lausekkeiden käyttö Elixirissa toteutetaan moniin funktioihin pakatuissa Regix-moduuleissa. Ne mahdollistavat monimutkaisten tekstipatternien luomisen, matchingin ja manipuloinnin.

## Katso myös:

Elixirin regex-moduulin dokumentaatio: https://hexdocs.pm/elixir/Regex.html

Opas Elixirin säännöllisten lausekkeiden käytöstä: https://elixirschool.com/en/lessons/advanced/regex/

Erlangin säännöllisten lausekkeiden viite: https://erlang.org/doc/man/re.html

PCRE:n kotisivu: http://www.pcre.org/
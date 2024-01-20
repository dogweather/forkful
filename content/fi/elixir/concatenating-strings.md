---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen yhdistäminen on toiminto, jossa yhdistetään kaksi tai useampi merkkijono yhdeksi. Ohjelmoijat tekevät tämän esimerkiksi dynaamisen sisällön luomiseksi tai tiedon esittämiseksi käyttäjälle yksinkertaisessa muodossa.

## Miten näin:

Elixirissä merkkijonot voidaan yhdistää käyttämällä <> operaattoria.

```Elixir
s1 = "Hei, "
s2 = "maailma!"
IO.puts s1 <> s2
```

Tämä antaa seuraavan tuloksen:

```Elixir
"Hei, maailma!"
```

## Syvempi sukellus:

Elixir perustuu Erlangiin, joka ei alun perin tukenut merkkijonojen yhdistämistä. Tämä on yksi syy, miksi Elixirin merkkijonojen yhdistäminen tehdään käyttämällä operaattoria <>, eikä traditioanlista  '+' operaattoria kuten joissakin muissa ohjelmointikielissä.

Elixirissa on operaattorien lisäksi myös funktioita merkkijonojen yhdistämistä varten. `String.concat/1` erikoistoiminto on esimerkiksi yksi tällainen vaihtoehto, se ottaa listan merkkijonoja ja yhdistää ne yhdeksi merkkijonoksi.

```Elixir
String.concat(["Hei, ", "maailma!"])
> "Hei, maailma!"
```

## Katso myös:

* Elixirin virallinen dokumentaatio merkkijonoista: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Elixir: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Päivämäärän Parsiminen Merkkijonosta Elixir-ohjelmointikielessä
Tässä artikkelissa opimme, miten päivämäärä parsitaan merkkijonosta Elixirin nykyversiolla. 

## Mikä & Miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa merkkijonossa olevan päivämäärän muuttamista päivämääräobjektiksi. Ohjelmoijat tarvitsevat tätä tietojen analysointiin ja tietojen järjestämiseen ajan mukaan.

## Kuinka:
Parsiminen Elixirissä voidaan tehdä `Date.from_iso8601/1` -funktiolla.

```elixir
{:ok, date} = Date.from_iso8601("2021-07-07")
IO.inspect(date)
```
Elixir palauttaa seuraavat teikstikonsolille:
```elixir
~D[2021-07-07]
```

## Syvällisempi katsaus
Elixirin päivämääräparsiminen perustuu ISO8601-standardiin, joka on kansainvälinen standardi päivämäärän ja ajan esittämiseen. On myös mahdollista käyttää muita kirjastoja, kuten Timex, monimutkaisempien päivämäärämuotojen käsittelyyn.
Joskus merkkijonopäivämäärän tulkinta voi olla virheellinen aikavyöhykkeen takia, joten on tärkeää tarkistaa aikavyöhykkeet datatiedostoissa.

## Katso myös
[Elixirin virallinen dokumentaatio päivämäärän parsimisesta](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)  
[Elixirin Timex-kirjaston dokumentaatio](https://hexdocs.pm/timex/readme.html)
[Elixirin School-tietosivusto](https://elixirschool.com/en/)
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

# Päivämäärän jäsentäminen merkkijonosta Elixir-kielessä

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärän luomista tekstitiedosta koodissa. Ohjelmoijat tekevät tämän, jotta he voivat hallita ja käyttää päivämääriä tehokkaasti koodissaan.

## Näin se tehdään:
```Elixir
Elixir-koodissa päivämäärän jäsentäminen merkkijonosta tehdään yleensä `Date.from_iso8601/1` -funktiolla. Tässä esimerkki:

```elixir
{:ok, date} = Date.from_iso8601("2021-09-15")
IO.inspect(date)
```

Koodin suorittamiseksi saat:
```elixir
~D[2021-09-15]
```

## Syvällisemmin
Historiallisesti päivämäärän jäsentäminen merkkijonosta on ollut osa ohjelmointia lähes sen alkumetreiltä saakka. Tämä herättää kysymyksen, onko muita tapoja jäsentää päivämäärä merkkijonosta.

Elixirissä on muitakin tapoja, kuten `DateTime.from_iso8601/1`, joka sisältää myös kellonajan. Elixirin sisäisesti päivämäärän käsittely perustuu Erlang-virtuaalikoneen kalenterimoduuliin, joka käsittelee ajan ISO8601-standardin mukaisesti.

## Katso myös
1. Elixir-lang virallinen dokumentaatio: [Date.from_iso8601/1](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
2. Elixir school - [Working with dates and times in Elixir](https://elixirschool.com/en/lessons/basics/date-and-time/)
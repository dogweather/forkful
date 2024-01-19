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

# Artikkeli: Päivämäärän haku merkkijonosta Elixir-ohjelmointikielellä

## Mikä & Miksi?
Päivämäärän haku merkkijonosta on prosessi, jossa päivämäärä eritellään ja muunnetaan merkkijonosta päivämääräobjektiksi. Ohjelmoijat tekevät tämän usein, jotta voisivat käsitellä päivämäärätietoja tehokkaammin ja helpommin.

## Kuinka tehdä:
Elixirin avulla voimme tehdä tämän helposti käyttämällä `DateTime` -moduulia.

```Elixir
# Esimerkki step-by-step
merkkijono = "2022-03-01 10:30:15"

# Otetaan Käyttöön DateTime
{:ok, datetime} = DateTime.from_iso8601(merkkijono)
IO.inspect(datetime)
```

Ajamisen jälkeen saamme tämän tuloksen:

```Elixir
~U[2022-03-01T10:30:15Z]
```

## Deep Dive
Historiallinen konteksti: Elixir käyttää peruskielensä Erlangin päivämäärän ja kellonajojen käsittelyä.

Vaihtoehdot: Voisit käyttää `Date` tai `Time` moduuleja, jos tarvitset vain päivämäärän tai ajan. Ne noudattavat samaa formaattia kuin `DateTime`.

Implementointi tiedot: `DateTime.from_iso8601/1` -funktio tulkitsee päivämäärän ISO 8601 -muotoon. Jos kaikki ei mene suunnitellusti, palautetaan virhekuvauksen sisältävä tuple.

## Katso myös:
1. [Elixir DateTime dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
2. [Elixirin tutustumispolku DateTime-toimintojen käyttöön](https://elixirschool.com/en/lessons/basics/date_time/)
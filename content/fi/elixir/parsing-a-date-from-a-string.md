---
title:                "Päivämäärän erottelu merkkijonosta."
html_title:           "Elixir: Päivämäärän erottelu merkkijonosta."
simple_title:         "Päivämäärän erottelu merkkijonosta."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Stringistä Päivämäärän Parsiminen Elixirissä

## Mikä & Miksi?

Päivämäärän parsiminen merkkijonosta tarkoittaa tiedon muuntamista string-muodosta järjestelmän ymmärtämään Date-tyyppiin. Tätä tarvitaan, kun käsitellään päivämääriä merkkijonojen muodossa, esimerkiksi tiedostoista, tietokannoista tai rajapinnoista saaduissa syötteissä.

## Näin teet:

```Elixir
String.to_charlist("2022-12-24")
|> Enum.map(&String.to_integer/1)
|> Date.new()
```

Tulos:

```bash
{:ok, ~D[2022-12-24]}
```

Ensin muunnetaan merkkijono charlistiksi, sitten jokainen osa muutetaan kokonaisluvuksi ja viimein uudeksi päivämääräksi.

## Syvällisempi tieto

Date.new-funktiota on käytetty Elixirin varhaisimmista versioista lähtien, koska se on perustavanlaatuinen päivämäärähallinnassa. `Date.from_iso8601/1` on myös hyvä vaihtoehto, jos syöte noudattaa ISO 8601 -päivämäärämuotoa.

Elixir käsittelee päivämäärät tuplena. Ensimmäinen elementti kuvaa vuotta, toinen kuukautta ja kolmas päivää.

## Katso myös

Lisätietoja saat seuraavista linkeistä:

- Elixirin virallinen dokumentaatio: [Date.new/1](https://hexdocs.pm/elixir/Date.html#new/1) ja [Date.from_iso8601/1](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
- Elixir School: [Date and Time Basics](https://elixirschool.com/en/lessons/basics/date_time/)
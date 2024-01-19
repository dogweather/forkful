---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsentäminen merkkijonosta tarkoittaa kelvollisen päivämääräobjektin luomista tekstiesityksestä. Ohjelmoijat tekevät tämän usein, koska tietoja tallennetaan ja lähetetään usein merkkijonoina.

## Miten:

Elixirin kanssa voit käyttää DateTime-muuntotoimintoja päivämäärän jäsentämiseen merkkijonosta. Tässä esimerkki:

```elixir
string_date = "2022-01-31 10:30:15Z"
{:ok, date_time} = DateTime.from_iso8601(string_date)
IO.inspect(date_time)
```

Tämä muuntaa merkkijonon DateTime-objektiksi. Tuotteen pitäisi näyttää näin:

```
#DateTime<2022-01-31 10:30:15Z>
```

## Syvempi sukellus:

Päivämäärän jäsentäminen merkkijonosta on ollut tarpeellinen taito ohjelmoinnissa sen varhaisista päivistä lähtien. Se on tärkeää, koska ihmiset ja järjestelmät ilmaisevat ja tallentavat päivämäärät ja ajat monin eri tavoin.

Elixirlang tarjoaa myös muita tapoja käsitellä merkkijonoja ja päivämääriä. Voit käyttää `NaiveDateTime.from_iso8601/2` ja `Date.from_iso8601/2` jos aikavyöhyke ei ole tarpeellinen tai käytettävissä.

Elixirin DateTime-muunnin toimii ISO 8601 muotoisten päivämäärien kanssa. ISO 8601 on kansainvälinen standardi, joka määrittelee päivämäärän ja ajan esitysmuodon.

## Katso myös:

1. [DateTime Elixir dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
2. [ISO 8601 Wikipedia](https://fi.wikipedia.org/wiki/ISO_8601)
3. [Elixir School - Däärien & ajan käsittely](https://elixirschool.com/en/lessons/basics/date_time/)
4. [Elixir Forum - DateTime jäsentäminen](https://elixirforum.com/t/parsing-date-time-values-from-string/22417)
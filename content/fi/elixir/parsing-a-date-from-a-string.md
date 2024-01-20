---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:29.776965-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Päivämäärän jäsentäminen merkkijonosta tarkoittaa, että muunnat tekstin päivämäärämuotoon, jota ohjelmat voivat käyttää. Ohjelmoijat tarvitsevat tätä toiminnallisuutta, kun heidän pitää käsitellä käyttäjien syötteitä, tallentaa päivämääriä tietokantoihin tai muotoilla päivämääriä käyttöliittymissä.

## How to (Kuinka tehdä):
Elixiriin sisältyy laaja valikoima päivämääräkäsittelytoimintoja. Käytetään `NaiveDateTime`-moduulia jäsentämiseen:

```elixir
# Jäsennetään ISO8601-muotoinen päivämäärä
date_string = "2023-04-05T14:30:05Z"
{:ok, naive_datetime} = NaiveDateTime.from_iso8601(date_string)
IO.inspect(naive_datetime)

# Määritetään muotoilu itse ja jäsentää y custom date string
custom_date_string = "05.04.2023"
{:ok, datetime, _} = DateTime.from_iso8601("#{custom_date_string}T00:00:00Z")
IO.inspect(datetime)
```

Esimerkki tulostaa:

```
~N[2023-04-05 14:30:05]
~U[2023-04-05 00:00:00Z]
```

Esim. vaativaan päivämääräkäsittelyyn katso `Timex`-kirjasto.

## Deep Dive (Syvä sukellus):
Päivämäärän jäsentäminen on osa ohjelmistosuunnittelua jo aikojen alusta. ISO8601-standardi luotiin selkiyttämään päivämäärien esitystä kansainvälisesti. Elixirin sisäänrakennetut moduulit, kuten `NaiveDateTime` ja `DateTime`, tarjoavat helpon tavan jäsentää ja muuttaa päivämääriä. `Timex`-kirjasto ottaa askeleen pidemmälle, tarjoten monipuolisempia toiminnallisuuksia ja formaatteja.

Elixir käyttää Erlangin `:calendar`-moduulia alapinnallaan, mikä tekee aikakäsittelystä tehokasta. Päivämäärästringit voidaan jäsentää useissa eri formaateissa, mutta ISO8601 on yleisesti käytetty standardi, koska se vähentää sekaannuksen mahdollisuuksia.

## See Also (Katso myös):
- Elixirin virallinen dokumentaatio `NaiveDateTime`- ja `DateTime`-moduuleista: https://hexdocs.pm/elixir/NaiveDateTime.html ja https://hexdocs.pm/elixir/DateTime.html
- Timex-kirjasto päivämäärä- ja aikakäsittelyyn: https://hex.pm/packages/timex
- ISO8601-päivämäärä ja aika -standardin yleiskatsaus: https://en.wikipedia.org/wiki/ISO_8601
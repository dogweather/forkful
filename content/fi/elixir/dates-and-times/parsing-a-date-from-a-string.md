---
title:                "Päivämäärän jäsentäminen merkkijonosta"
aliases:
- /fi/elixir/parsing-a-date-from-a-string.md
date:                  2024-01-28T02:05:14.592460-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän jäsennys merkkijonosta tarkoittaa tekstin, kuten "2023-04-05", muuntamista päivämäärämuotoon, jonka ohjelmasi voi ymmärtää ja käsitellä. Ohjelmoijat tekevät näin, koska päivämääriä on monenlaisissa muodoissa, ja niiden yhdenmukaisuus on tarpeen vertailua, lajittelua tai tallentamista varten.

## Kuinka:

Elixirissä voit jäsentää päivämääriä käyttäen `Date`-moduulia. Näin muunnat merkkijonon päivämääräksi:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Esimerkkituloste:

```elixir
~D[2023-04-05]
```

Eri muotojen käsittelyyn voit käyttää `Timex`-kirjastoa:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Esimerkkituloste:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Syväsukellus

`Date.from_iso8601/1`-funktio on osa Elixiring vakio kirjastoa, joka on tuotu käyttöön helpottamaan ISO8601-päivämäärästandaardin - yleisen päivämäärämuodon - jäsentämistä. Mutta elämä ei ole niin yksinkertaista; päivämääriä on valtavasti eri muodoissa. Tässä `Timex`, kolmannen osapuolen Elixir-kirjasto, astuu peliin. Se on rikkaampi kuin sisäänrakennetut Elixiring päivämäärätoiminnot ja auttaa käsittelemään laajan valikoiman päivämäärämuotoja.

Elixir itsessään on muuttumaton, mikä tarkoittaa, että jäsenneet päivämäärät eivät ole poikkeus; niitä ei voi muuttaa luomisen jälkeen. Tämä ominaisuus palauttaa mieleen Elixiring funktionaalisen ohjelmoinnin juuret, taaten ennustettavuuden ja helpomman vianetsinnän.

Historiallisesti päivämäärien jäsentäminen on ollut vaikeaa vaihtelevien standardien vuoksi. Kuitenkin kirjastojen, kuten `Timex`, ja Elixiring kielen ominaisuuksien avulla monimutkaisuus on piilotettu, tehdään kehittäjän elämästä hieman yksinkertaisempaa.

## Katso Myös

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Timex Dokumentaatio](https://hexdocs.pm/timex/Timex.html)
- [ISO8601 Standardi](https://www.iso.org/iso-8601-date-and-time-format.html)

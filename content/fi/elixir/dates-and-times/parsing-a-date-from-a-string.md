---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:14.592460-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta tarkoittaa tekstin,\
  \ kuten \"2023-04-05\", muuntamista p\xE4iv\xE4m\xE4\xE4r\xE4muotoon, jonka ohjelmasi\
  \ voi ymm\xE4rt\xE4\xE4 ja k\xE4sitell\xE4.\u2026"
lastmod: '2024-03-13T22:44:56.236440-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta tarkoittaa tekstin,\
  \ kuten \"2023-04-05\", muuntamista p\xE4iv\xE4m\xE4\xE4r\xE4muotoon, jonka ohjelmasi\
  \ voi ymm\xE4rt\xE4\xE4 ja k\xE4sitell\xE4.\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta"
weight: 30
---

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

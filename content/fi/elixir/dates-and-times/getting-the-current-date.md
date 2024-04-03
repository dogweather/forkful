---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:36.803762-07:00
description: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n saaminen Elixirissa k\xE4sitt\xE4\
  \xE4 j\xE4rjestelm\xE4n p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan tiedon saavuttamisen,\
  \ yleinen teht\xE4v\xE4 lokitukseen, datan leimaamiseen\u2026"
lastmod: '2024-03-13T22:44:56.237609-06:00'
model: gpt-4-0125-preview
summary: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n saaminen Elixirissa k\xE4sitt\xE4\xE4\
  \ j\xE4rjestelm\xE4n p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan tiedon saavuttamisen, yleinen\
  \ teht\xE4v\xE4 lokitukseen, datan leimaamiseen tai mihin tahansa toimintoon, joka\
  \ edellytt\xE4\xE4 nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n tuntemista."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
Elixirin standardikirjaston, `DateTime` moduulin kautta, voi hakea nykyisen päivämäärän ja ajan. Koska Elixir toimii Erlang VM (BEAM) päällä, se hyödyntää taustalla olevia Erlangin ajan käsittelyn toiminnallisuuksia.

### Käyttäen Elixirin Standardikirjastoa
Elixir tarjoaa `DateTime.utc_now/0` funktion nykyisen päivämäärän ja ajan saamiseksi UTC:ssä.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Esimerkkituloste:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Saat nykyisen päivämäärän erikseen, sinun tulee eristää vuoden, kuukauden ja päivän osat:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Esimerkkituloste:**
```
~D[2023-05-04]
```

### Käyttäen Timex-kirjastoa
Monimutkaisempiin päivämäärä-aika vaatimuksiin voi hyödyntää suosittua kolmannen osapuolen kirjastoa nimeltä Timex. Lisää ensin `Timex` riippuvuudeksi mix.exs tiedostoosi:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Riippuvuuden asentamisen jälkeen (`mix deps.get`), voit käyttää Timexia nykyisen päivämäärän saamiseksi:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Esimerkkituloste:**
```
~D[2023-05-04]
```

Timex tarjoaa laajat toiminnallisuudet päivämäärän-aika manipulaatiolle, tehden siitä tehokkaan lisän Elixir-sovelluksiisi, erityisesti kun käsitellään aikavyöhykkeitä, muotoilua ja päivämäärien sekä aikojen jäsentämistä.

Ymmärtämällä ja hyödyntämällä Elixirin sisäänrakennettuja kyvykkyyksiä sekä Timex-kirjastoa, voit helposti käsitellä päivämääriä ja aikoja Elixir-sovelluksissasi, räätälöiden kokemuksen sovelluksesi tarpeiden mukaan tarkkuudella ja vaivattomuudella.

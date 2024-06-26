---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:11.910023-07:00
description: 'Hoe: In Elixir heeft de `Date`-module een `to_string/1`-functie die
  een datum omzet naar een string.'
lastmod: '2024-03-13T22:44:50.473624-06:00'
model: gpt-4-0125-preview
summary: In Elixir heeft de `Date`-module een `to_string/1`-functie die een datum
  omzet naar een string.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
In Elixir heeft de `Date`-module een `to_string/1`-functie die een datum omzet naar een string.

```elixir
date = ~D[2023-03-14]
date_string = Date.to_string(date)
IO.puts(date_string)  # "2023-03-14"
```

Voor meer aangepaste opmaak kun je `Timex` gebruiken:
```elixir
{:ok, datetime} = DateTime.new(~D[2023-03-14], {0, 0, 0})
formatted_date = Timex.format!(datetime, "{YYYY}-{0M}-{0D}")
IO.puts(formatted_date)  # "2023-03-14"
```

## Diepere Duik
Voor Elixir 1.3 was datum- en tijdsmanipulatie omslachtiger en afhankelijk van externe bibliotheken. Met versie 1.3 en daarna heeft Elixir de `Date`, `Time` en `DateTime` modules opgenomen voor een betere afhandeling van datums en tijden.

Wanneer je opmaak nodig hebt die verder gaat dan de ISO8601-standaard, overweeg dan de `Timex`-bibliotheek, een Elixir-pakket dat een complete ervaring voor datum- en tijdsbehandeling biedt.

Omzetten naar een string is geen magie. Het gaat erom de complexe `Date`-struct om te zetten in iets universeel begrijpelijks. Een struct bevat meer informatie dan de stringrepresentatie, dus wees ervan bewust dat het terugconverteren van een string naar een datum deze extra context zal verliezen, tenzij deze op de juiste manier is gecodeerd.

## Zie Ook
- Elixir Date-module: https://hexdocs.pm/elixir/Date.html
- Timex-documentatie: https://hexdocs.pm/timex/readme.html
- ISO8601-formaat: https://nl.wikipedia.org/wiki/ISO_8601

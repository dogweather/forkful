---
title:                "Een datum converteren naar een string"
aliases:
- /nl/elixir/converting-a-date-into-a-string/
date:                  2024-01-28T21:57:11.910023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum omzetten naar een string in Elixir verandert de datum van een struct naar een leesbare reeks tekens voor weergave of opslag. Programmeurs doen dit om tijdstempels te registreren, datums weer te geven in templates, of om data te serialiseren voor communicatie met externe diensten.

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

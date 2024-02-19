---
aliases:
- /nl/elixir/parsing-a-date-from-a-string/
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:01.302887-07:00
description: "Een datum uit een string parsen gaat over het nemen van tekst, zoals\
  \ \"2023-04-05\", en het omzetten naar een datumformaat dat je programma kan begrijpen\
  \ en\u2026"
lastmod: 2024-02-18 23:09:01.536987
model: gpt-4-0125-preview
summary: "Een datum uit een string parsen gaat over het nemen van tekst, zoals \"\
  2023-04-05\", en het omzetten naar een datumformaat dat je programma kan begrijpen\
  \ en\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum uit een string parsen gaat over het nemen van tekst, zoals "2023-04-05", en het omzetten naar een datumformaat dat je programma kan begrijpen en mee kan werken. Programmeurs doen dit omdat datums in tal van formaten voorkomen, en ze consistentie nodig hebben om ze goed te kunnen vergelijken, sorteren of opslaan.

## Hoe:

In Elixir kun je datums parsen met behulp van de `Date` module. Zo zet je een string om in een datum:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Voorbeelduitvoer:

```elixir
~D[2023-04-05]
```

Om verschillende formaten te kunnen hanteren, kun je de `Timex` bibliotheek gebruiken:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Voorbeelduitvoer:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Diepduiken

De functie `Date.from_iso8601/1` is onderdeel van Elixir's standaardbibliotheek, geïntroduceerd om het gemakkelijk parsen van de ISO8601 datumstandaard - een gangbare datumnotatie - te waarborgen. Maar het leven is niet zo simpel; datums komen in talloze formaten voor. Daarom is er `Timex`, een bibliotheek van derden voor Elixir, die rijker is dan de ingebouwde Elixir datumfuncties en helpt bij het hanteren van een breed scala aan datumformaten.

Elixir zelf is onveranderlijk, wat betekent dat geparseerde datums geen uitzondering zijn; ze kunnen niet worden gewijzigd nadat ze zijn gemaakt. Deze eigenschap is terug te voeren op de functionele programmeerwortels van Elixir, wat voorspelbaarheid en eenvoudiger debugging garandeert.

Historisch gezien was het parsen van datums lastig vanwege uiteenlopende standaarden. Maar met bibliotheken zoals `Timex` en taalfuncties in Elixir wordt de complexiteit weggeabstraheerd, wat het leven van een ontwikkelaar een beetje eenvoudiger maakt.

## Zie Ook

- [Elixir Datum](https://hexdocs.pm/elixir/Date.html)
- [Timex Documentatie](https://hexdocs.pm/timex/Timex.html)
- [ISO8601-standaard](https://www.iso.org/iso-8601-date-and-time-format.html)

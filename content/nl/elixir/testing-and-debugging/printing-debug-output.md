---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:31.776699-07:00
description: "Het afdrukken van debugoutput in Elixir omvat het weergeven van tussentijdse\
  \ resultaten of variabele waarden in de console. Programmeurs doen dit om\u2026"
lastmod: '2024-02-25T18:49:47.857314-07:00'
model: gpt-4-0125-preview
summary: "Het afdrukken van debugoutput in Elixir omvat het weergeven van tussentijdse\
  \ resultaten of variabele waarden in de console. Programmeurs doen dit om\u2026"
title: Debug-output afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?

Het afdrukken van debugoutput in Elixir omvat het weergeven van tussentijdse resultaten of variabele waarden in de console. Programmeurs doen dit om fouten op te sporen of om te begrijpen wat hun code doet op een bepaald punt in de uitvoering.

## Hoe doe je dat:

```elixir
defmodule DebugVoorbeeld do
  def toon_debug_output do
    naam = "Elixir"

    IO.inspect(naam, label: "Debug")
    # verdere verwerking
  end
end

DebugVoorbeeld.toon_debug_output()
# Uitvoer:
# Debug: "Elixir"
```

Dit toont de eenvoudigste manier om iets naar de console te printen met `IO.inspect/2`. De label-optie voegt een aangepast voorvoegsel toe, waardoor de output makkelijker te herkennen is.

## Diepgaand

Elixir's `IO.inspect/2` functie is vergelijkbaar met `puts` in Ruby of `console.log` in JavaScript. Het is geweldig voor snelle en vuile debugging, een praktijk zo oud als programmeren zelf.

Alternatieven in Elixir omvatten het gebruik van de `Logger`-module voor systematischer applicatieniveau-logboekregistratie. Dit is configureerbaarder en geschikter voor productie.

Voor implementatiedetails geeft `IO.inspect/2` de gegeven data terug, waardoor het eenvoudig in een pipeline kan worden ingevoegd zonder de functionaliteit te beïnvloeden. Historisch gezien heeft Elixir altijd nadruk gelegd op ontwikkelaarstools, en functies zoals `IO.inspect/2` belichamen dit door het debuggen een meer geïntegreerde ervaring te maken.

## Zie ook

- Elixir's IO-module: https://hexdocs.pm/elixir/IO.html
- Introductie tot debugging in Elixir: https://elixirschool.com/en/lessons/specifics/debugging
- Officiële gids voor Logger: https://hexdocs.pm/logger/Logger.html

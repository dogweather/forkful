---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:31.776699-07:00
description: 'Hoe doe je dat: .'
lastmod: '2024-03-13T22:44:50.464734-06:00'
model: gpt-4-0125-preview
summary: .
title: Debug-output afdrukken
weight: 33
---

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

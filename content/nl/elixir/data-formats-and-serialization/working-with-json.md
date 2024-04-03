---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:23.853409-07:00
description: "JSON (JavaScript Object Notatie) is een lichtgewicht data-uitwisselingsformaat\
  \ dat gemakkelijk te lezen en schrijven is voor mensen en gemakkelijk te\u2026"
lastmod: '2024-03-13T22:44:50.483270-06:00'
model: gpt-4-0125-preview
summary: JSON (JavaScript Object Notatie) is een lichtgewicht data-uitwisselingsformaat
  dat gemakkelijk te lezen en schrijven is voor mensen en gemakkelijk te ontleden
  en genereren is voor machines.
title: Werken met JSON
weight: 38
---

## Hoe:
Om met JSON in Elixir om te gaan, gebruiken we bibliotheken zoals `Jason` of `Poison`. Hier is een snelle handleiding met `Jason`:

```elixir
# Voeg Jason toe aan je mix.exs als een afhankelijkheid
{:jason, "~> 1.3"}

# in een .ex bestand, om Elixir naar JSON te coderen
json_string = Jason.encode!(%{foo: "bar"})

# Nu JSON decoderen naar Elixir
elixir_map = Jason.decode!(json_string)
```

Uitvoer:

```elixir
json_string #=> "{\"foo\":\"bar\"}"
elixir_map  #=> %{"foo" => "bar"}
```

Coderen met `opts` voor mooi printen:

```elixir
Jason.encode!(%{foo: "bar"}, pretty: true)
```

Uitvoer:

```json
{
  "foo": "bar"
}
```

## Diepe Duik
JSON werd voorgesteld door Douglas Crockford in het begin van de jaren 2000. Het kreeg snel adoptie vanwege zijn eenvoud ten opzichte van XML.

Alternatieven? Zeker—XML, YAML, of Protocol Buffers, maar JSON heerst vanwege de eenvoud en native ondersteuning in JavaScript.

Onder de motorkap converteren JSON-bibliotheken Elixir-datatypes naar JSON-strings en vice versa. Elixir's patroon matching en robuuste standaard bibliotheek maken het coderings- en decoderingsproces soepel.

## Zie Ook
- Jason GitHub: https://github.com/michalmuskala/jason
- Poison GitHub: https://github.com/devinus/poison
- Elixir School JSON lessen: https://elixirschool.com/en/lessons/specifics/jason/

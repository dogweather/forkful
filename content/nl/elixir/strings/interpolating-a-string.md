---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:49.007946-07:00
description: "Stringinterpolatie stelt je in staat om variabelen of berekeningen in\
  \ een string in te voegen. Het is handig voor het dynamisch opbouwen van strings\u2026"
lastmod: '2024-03-13T22:44:50.448180-06:00'
model: gpt-4-0125-preview
summary: "Stringinterpolatie stelt je in staat om variabelen of berekeningen in een\
  \ string in te voegen. Het is handig voor het dynamisch opbouwen van strings\u2026"
title: Een string interpoleren
weight: 8
---

## Wat & Waarom?
Stringinterpolatie stelt je in staat om variabelen of berekeningen in een string in te voegen. Het is handig voor het dynamisch opbouwen van strings zonder de rommel van concatenatie.

## Hoe:
```elixir
name = "Josie"
age = 28

# Variabelen interpoleren
greeting = "Hallo, #{name}! Je bent #{age} jaar oud."
IO.puts greeting
```
Voorbeelduitvoer:
```
Hallo, Josie! Je bent 28 jaar oud.
```
```elixir
# Expressies interpoleren
IO.puts "Over vijf jaar zal #{name} #{age + 5} jaar oud zijn."
```
Voorbeelduitvoer:
```
Over vijf jaar zal Josie 33 jaar oud zijn.
```

## Diepgaande Duik
In de vroege dagen plakte je strings aan elkaar vast met `+` of `,`. Het was een pijn. Talen begonnen toen interpolatie te gebruiken voor een schonere, meer leesbare benadering. Elixir, als een moderne taal, ondersteunt deze functie ook native.

Dit gebeurt er achter de schermen met `"Hallo, #{name}!"`: tijdens de compilatie transformeert Elixir de string naar een concatenatie van binaire delen, wat efficiënt is omdat binaries in Elixir onveranderlijk zijn.

Alternatieve manieren om in Elixir met strings om te gaan zonder interpolatie kunnen het gebruik van de `String.concat/2` of de `<>` operator omvatten, maar deze methoden zijn minder ergonomisch voor complexe strings.

De interpolatiesyntaxis `"#{...}"` kan elke Elixir-uitdrukking bevatten, die wordt geëvalueerd en vervolgens omgezet naar een string. Dit is mogelijk omdat Elixir dynamisch getypeerd is en eersteklas ondersteuning voor uitdrukkingen in zijn strings heeft. Maar onthoud, het is het beste om dit voor eenvoudigere uitdrukkingen te bewaren om leesbaarheid te behouden.

## Zie Ook
- Documentatie van Elixir's `String` module: https://hexdocs.pm/elixir/String.html
- Een gids tot Elixir's binaire datatype: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html

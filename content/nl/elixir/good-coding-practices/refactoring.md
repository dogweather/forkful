---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:00.808032-07:00
description: "Refactoring is het proces van het herstructureren van bestaande code\
  \ zonder het externe gedrag ervan te veranderen, gericht op het verbeteren van niet-\u2026"
lastmod: '2024-02-25T18:49:47.863372-07:00'
model: gpt-4-0125-preview
summary: "Refactoring is het proces van het herstructureren van bestaande code zonder\
  \ het externe gedrag ervan te veranderen, gericht op het verbeteren van niet-\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande code zonder het externe gedrag ervan te veranderen, gericht op het verbeteren van niet-functionele attributen zoals leesbaarheid en onderhoudbaarheid. Programmeurs doen dit om de code schoner, gemakkelijker te begrijpen en efficiënter te maken, wat toekomstige updates vergemakkelijkt en het risico op bugs vermindert.

## Hoe te:
Laten we een veelvoorkomend Elixir-patroon opruimen. We gaan een functie `calculate_stats` refactoren die meer doet dan het zou moeten door het in kleinere, herbruikbare stukken te breken.

```elixir
defmodule Stats do
  # Originele, niet-gerestructureerde code
  def calculate_stats(data) do
    totaal = Enum.sum(data)
    aantal = Enum.count(data)
    gemiddelde = totaal / aantal
    {gemiddelde, totaal}
  end
  
  # Gerefactorde code
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    gemiddelde = calculate_mean(data)
    totaal = calculate_total(data)
    {gemiddelde, totaal}
  end
end

# Voorbeelduitvoer
# Voor Refactoring
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Na Refactoring
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Zoals je kunt zien, blijft de uitvoer hetzelfde, maar nu hebben we modulaire functies die onafhankelijk kunnen worden hergebruikt en getest.

## Diepere Duik
Refactoring is geen nieuw concept; het is sinds de vroege dagen van softwareontwikkeling een cruciaal onderdeel van programmeren geweest. Opmerkelijke werken, zoals Martin Fowlers "Refactoring: Improving the Design of Existing Code", bieden fundamentele praktijken voor refactoring met inzichten in wanneer en hoe ze toe te passen.

Alternatieven voor handmatige refactoring omvatten geautomatiseerde codeanalysetools, die suggesties voor refactoring kunnen doen of zelfs refactoring kunnen uitvoeren. Echter, geautomatiseerde tools begrijpen mogelijk niet altijd de volledige context van de code en kunnen subtiliteiten missen die een menselijke beoordelaar zou vangen.

Implementatiedetails in Elixir specifiek omvatten het begrijpen van het functionele paradigma en het gebruik van patroonmatching, guard clauses en de pipe-operator om duidelijke en bondige code te schrijven. Refactoring omvat vaak het converteren van complexe imperatieve-stijlfuncties naar kleinere, samenstelbare functies die de voorkeur geven aan Elixir's voorkeur voor onveranderlijkheid en operaties zonder bijeffecten.

## Zie Ook
Voor meer over Elixir-specifieke refactoringtechnieken:

- [Elixir's officiële gidsen](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" van Martin Fowler](https://martinfowler.com/books/refactoring.html), voor algemene principes die kunnen worden toegepast op Elixir.
- [Credo, een statische code-analysedtool voor Elixir](https://github.com/rrrene/credo) die beste praktijken aanmoedigt.
- [Exercism Elixir Track](https://exercism.org/tracks/elixir), voor praktische oefeningen die vaak refactoring omvatten.

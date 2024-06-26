---
date: 2024-01-26 01:18:23.770597-07:00
description: "Hvordan: La oss rydde opp i et vanlig Elixir-m\xF8nster. Vi vil refaktorere\
  \ en funksjon `calculate_stats` som gj\xF8r mer enn den burde ved \xE5 bryte den\
  \ ned i\u2026"
lastmod: '2024-03-13T22:44:40.451820-06:00'
model: gpt-4-0125-preview
summary: "La oss rydde opp i et vanlig Elixir-m\xF8nster."
title: Refaktorering
weight: 19
---

## Hvordan:
La oss rydde opp i et vanlig Elixir-mønster. Vi vil refaktorere en funksjon `calculate_stats` som gjør mer enn den burde ved å bryte den ned i mindre, gjenbrukbare deler.

```elixir
defmodule Stats do
  # Opprinnelig, urefaktorert kode
  def calculate_stats(data) do
    total = Enum.sum(data)
    antall = Enum.count(data)
    gjennomsnitt = total / antall
    {gjennomsnitt, total}
  end
  
  # Refaktorert kode
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    gjennomsnitt = calculate_mean(data)
    total = calculate_total(data)
    {gjennomsnitt, total}
  end
end

# Eksempel på resultat
# Før refaktorering
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Etter refaktorering
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Som du kan se, forblir resultatet det samme, men nå har vi modulære funksjoner som kan gjenbrukes og testes uavhengig.

## Dypdykk
Refaktorering er ikke et nytt konsept; det har vært en avgjørende del av programmering siden de tidlige dagene av programvareutvikling. Fremtredende verker, som Martin Fowlers "Refactoring: Improving the Design of Existing Code", gir grunnleggende praksiser for refaktorering med innsikt i når og hvordan de skal brukes.

Alternativer til manuell refaktorering inkluderer automatiserte kodeanalyseverktøy, som kan foreslå eller til og med utføre refaktoreringer. Imidlertid kan automatiserte verktøy ikke alltid forstå hele konteksten av koden og kan gå glipp av nyanser som en menneskelig gjennomgåer ville fange opp.

Implementeringsdetaljer i Elixir spesifikt inkluderer å forstå det funksjonelle paradigmet og utnytte mønstermatching, vaktsetninger og rør-operatøren for å skrive klar og konsis kode. For eksempel innebærer refaktorering ofte å konvertere komplekse imperativ-stilfunksjoner til mindre, sammensetbare funksjoner som følger Elixirs preferanse for immutabilitet og operasjoner fri for sideeffekter.

## Se også
For mer om Elixir-spesifikke refaktoreringsmetoder:

- [Elixirs offisielle guider](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" av Martin Fowler](https://martinfowler.com/books/refactoring.html), for generelle prinsipper som kan anvendes på Elixir.
- [Credo, et statisk kodeanalyseverktøy for Elixir](https://github.com/rrrene/credo) som oppmuntrer til beste praksis.
- [Exercism Elixir Track](https://exercism.org/tracks/elixir), for praktiske øvelser som ofte involverer refaktorering.

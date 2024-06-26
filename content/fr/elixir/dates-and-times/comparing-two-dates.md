---
date: 2024-01-20 17:32:36.906136-07:00
description: "Comment faire : Historiquement, Elixir, construit sur Erlang, simplifie\
  \ la manipulation des dates. Avant Elixir 1.3, on s'appuyait sur des librairies\u2026"
lastmod: '2024-04-05T21:53:58.919248-06:00'
model: gpt-4-1106-preview
summary: Historiquement, Elixir, construit sur Erlang, simplifie la manipulation des
  dates.
title: Comparer deux dates
weight: 27
---

## Comment faire :
```elixir
# On utilise NaiveDateTime pour des dates sans fuseau horaire
date1 = ~N[2023-03-15 14:00:00]
date2 = ~N[2023-03-18 14:00:00]

# Comparer les dates directement
date1 < date2
# Résultat : true

# Calculer l'écart en secondes
DateTime.diff(date2, date1)
# Résultat : 259200

# Formater les dates si nécessaire
date1_formatted = NaiveDateTime.to_string(date1)
# Résultat : "2023-03-15 14:00:00"
```

## Plongée profonde
Historiquement, Elixir, construit sur Erlang, simplifie la manipulation des dates. Avant Elixir 1.3, on s'appuyait sur des librairies tierces, mais maintenant, les modules `NaiveDateTime` et `DateTime` font partie de l'écosystème standard. Alternativement, on peut utiliser `Timex`, une librairie puissante pour le temps. La précision des comparaisons dépend de l'information temporelle disponible : sans fuseau horaire (`NaiveDateTime`) ou avec (`DateTime`).

## Pour aller plus loin
Documentation Elixir officielle sur NaiveDateTime : https://hexdocs.pm/elixir/NaiveDateTime.html

Documentation Elixir officielle sur DateTime : https://hexdocs.pm/elixir/DateTime.html

GitHub de la librairie Timex : https://github.com/bitwalker/timex

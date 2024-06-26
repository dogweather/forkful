---
date: 2024-01-20 17:28:36.734807-07:00
description: "Comment faire : Historiquement, manipuler des dates n'a jamais \xE9\
  t\xE9 simple, surtout avec les variations de fuseaux horaires ou les r\xE8gles de\
  \ l'heure d'\xE9t\xE9.\u2026"
lastmod: '2024-04-05T21:53:58.920350-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, manipuler des dates n'a jamais \xE9t\xE9 simple, surtout\
  \ avec les variations de fuseaux horaires ou les r\xE8gles de l'heure d'\xE9t\xE9\
  ."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```elixir
# Ajouter 5 jours à la date actuelle
date_today = Date.utc_today()
date_future = Date.add(date_today, 5)
IO.puts(Date.to_string(date_future))
```

```elixir
# Soustraire 30 jours à une date spécifique
date_specific = ~D[2023-04-01]
date_past = Date.add(date_specific, -30)
IO.puts(Date.to_string(date_past))
```

## Plongée Profonde:
Historiquement, manipuler des dates n'a jamais été simple, surtout avec les variations de fuseaux horaires ou les règles de l'heure d'été. En Elixir, on utilise le module `Date` qui abstrait ces complexités. Comme alternatives, on a les librairies tierces comme `Timex`, mais le module `Date` intégré est souvent suffisant et plus simple à maintenir. Il utilise le calendrier du système sous-jacent et prend en charge l'arithmétique des dates, en se basant sur des durées de temps bien définies (jours, mois, années).

## Voir Aussi :
- La [documentation officielle de Elixir sur Date](https://hexdocs.pm/elixir/Date.html)
- Le chapitre sur [les datetimes dans le guide de démarrage Elixir](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html) pour plus de contexte.

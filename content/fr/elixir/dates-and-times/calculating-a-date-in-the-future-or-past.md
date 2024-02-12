---
title:                "Calcul d'une date future ou passée"
aliases: - /fr/elixir/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:36.734807-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Calculer une date dans le futur ou le passé, c'est ajuster une date donnée en y ajoutant ou soustrayant des jours, des mois, etc. Les programmeurs le font pour gérer des échéances, des périodes d'abonnement, des rappels, et d'autres fonctionnalités liées au temps.

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

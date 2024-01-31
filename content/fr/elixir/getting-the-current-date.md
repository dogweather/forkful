---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:13:58.836125-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Obtenir la date actuelle est essentiel pour horodater les événements, analyser les tendances temporelles ou simplement afficher la date d'aujourd'hui. Les programmeurs utilisent cette fonction pour tout, de la validation de la durée de vie des tokens à la planification des tâches récurrentes.

## How to: (Comment faire : )
Elixir rend la capture de la date actuelle assez simple avec le module `DateTime`.

```elixir
# Obtenir la date et l'heure courantes en UTC
DateTime.utc_now()
# => #DateTime<2023-04-06 15:34:25.123456Z>

# Pour obtenir la date actuelle dans un fuseau horaire spécifique, utilisez Timezone 
# (après avoir ajouté `:tzdata` à vos dépendances)
{:ok, paris_datetime} = DateTime.now("Europe/Paris")
paris_datetime
# => #DateTime<2023-04-06 17:34:25.123456+02:00 Europe/Paris>
```

## Deep Dive (Plongée en profondeur)
L'importance de gérer le temps correctement dans la programmation ne peut être sous-estimée - des bugs comme le problème de l'an 2000 rappellent les pièges d'une mauvaise gestion du temps. Historiquement, Elixir s'appuie sur l'Erlang VM, qui offre d'excellentes fonctionnalités de gestion du temps. `DateTime` est maintenant la manière standard en Elixir de manipuler les dates et les heures. Les alternatives incluent l'ancien module `:calendar` d'Erlang ou des bibliothèques tierces comme `Timex`, qui peuvent offrir des fonctionnalités supplémentaires.

## See Also (Voir Aussi)
Pour aller plus loin, voici quelques liens utiles :

- Elixir `DateTime` documentation: https://hexdocs.pm/elixir/DateTime.html
- Erlang `:calendar` module: http://erlang.org/doc/man/calendar.html
- `Timex` documentation: https://hexdocs.pm/timex/Timex.html
- Working with time zones in Elixir: https://elixirschool.com/en/lessons/basics/date_time/

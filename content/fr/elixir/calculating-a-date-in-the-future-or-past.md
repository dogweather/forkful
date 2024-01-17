---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Elixir: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?
Calculer une date dans le futur ou dans le passé est une fonctionnalité couramment utilisée en programmation qui permet de déterminer une date qui se situe à une certaine distance dans le temps par rapport à une date de référence. Les programmeurs utilisent cette fonctionnalité pour planifier des tâches, valider des données ou pour d'autres besoins spécifiques.

# Comment faire:
Il existe différentes manières de calculer une date dans le futur ou dans le passé en utilisant Elixir. Vous pouvez utiliser la bibliothèque standard Elixir `DateTime` ou la bibliothèque tierce `Timex`, qui offre des fonctionnalités plus avancées. Voici un exemple d'implémentation avec `DateTime`:

```
selon obtenir_date (date) do
  date
  |> DateTime.from_iso8601! ()
  |> DateTime.shift (days: 7)
  |> DateTime.to_iso8601! ()
fin
```

Le résultat devrait être une date qui se situe sept jours après la date de référence. Vous pouvez également utiliser des fonctions telles que `DateTime.add/3` pour ajouter ou soustraire des jours, mois ou années à une date.

# Deep Dive:
L'API `DateTime` a été ajoutée en 2014 lors de la sortie de la version 1.0 d'Elixir. Avant cela, il fallait utiliser des bibliothèques tierces pour gérer les dates et les heures. La bibliothèque `Timex` offre plus de fonctionnalités que `DateTime`, telles que la prise en charge de fuseaux horaires et de formats de date personnalisés.

Dans le cas où vous auriez besoin de gérer des dates avant l'ère Unix (1970), Elixir offre également la bibliothèque `Calendar` qui prend en charge les dates du calendrier julien et grégorien.

# Voir aussi:
- Documentation de la bibliothèque standard Elixir `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Documentation de la bibliothèque tierce `Timex`: https://hexdocs.pm/timex/
- Documentation de la bibliothèque `Calendar`: https://hexdocs.pm/calendar/
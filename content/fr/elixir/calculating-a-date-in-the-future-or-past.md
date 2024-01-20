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

## Qu'est-ce que c'est et pourquoi ?

Calculer une date dans le futur ou le passé consiste à ajouter ou à soustraire un certain nombre de jours, mois ou années à une date donnée. Les programmeurs le font pour résoudre une variété de problèmes, y compris le suivi des délais, la planification des événements et la gestion des horodatages.

## Comment ça marche:

Elixir utilise le module "DateTime" pour gérer les dates et le temps. Voyons cela en action:

```elixir
# pour obtenir la date actuelle
date_aujourdhui = DateTime.utc_now()

elixir> IO.inspect(date_aujourdhui)
"2022-03-01T09:35:09Z"

# Pour ajouter une semaine à la date d'aujourd'hui
semaine_prochaine = DateTime.add(date_aujourdhui, 7*24*60*60, :second)

elixir> IO.inspect(semaine_prochaine)
"2022-03-08T09:35:09Z"

# Pour soustraire un mois de la date d'aujourd'hui
mois_dernier = DateTime.add(date_aujourdhui, -30*24*60*60, :second) 

elixir> IO.inspect(mois_dernier)
"2022-02-14T09:35:09Z"
```

## Regardons plus en profondeur:

Le concept de calculer une date dans le passé ou le futur existait bien avant que l'informatique ne devienne une chose. C'est la base de tout, de l'astrologie à la planification des récoltes.

Pour un autre chemin, vous pouvez utiliser le module `NaiveDateTime`. Il est plus rapide mais n’applique pas les ajustements de fuseau horaire et de DST. En ce qui concerne les détails de mise en oeuvre, le calendrier de la bibliothèque standard d’Elixir traite des dates en utilisant le calendrier grégorien.

## Voir aussi:
- [Documentation officielle d'Elixir DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Guide Elixir pour travailler avec les dates et les heures](https://elixirschool.com/en/lessons/basics/date_time/) 
- [API Elixir NaiveDateTime](https://hexdocs.pm/elixir/NaiveDateTime.html)
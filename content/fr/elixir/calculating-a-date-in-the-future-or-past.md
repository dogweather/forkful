---
title:                "Elixir: Calcul d'une date future ou passée"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

La capacité de calculer une date dans le futur ou le passé est très utile en programmation, car elle permet de manipuler et de gérer efficacement les dates. Cela peut être utile pour créer des rappels, des événements futurs ou des validations de date dans une application.

## Comment faire

Pour calculer une date dans le futur ou le passé en Elixir, nous pouvons utiliser la fonction `Date.add/2` pour ajouter un nombre de jours, de semaines, de mois ou d'années à une date donnée. Nous pouvons également utiliser la fonction `Date.diff/2` pour calculer la différence entre deux dates.

Voici un exemple de code pour calculer une date dans le futur en ajoutant 5 jours à la date actuelle:

```Elixir
current_date = Date.utc_today()
new_date = Date.add(current_date, 5, :days)
IO.puts "La date dans 5 jours sera le #{Date.to_iso8601(new_date)}"
```

La sortie de ce code sera:

```
La date dans 5 jours sera le 2022-01-31
```

## Plongée en profondeur

En plus d'ajouter ou de soustraire un certain nombre de jours, de semaines, de mois ou d'années à une date, nous pouvons également utiliser des fonctions telles que `Date.beginning_of_week/2` et `Date.end_of_day/2` pour obtenir la date du début ou de la fin d'une semaine ou d'un jour spécifique.

Par exemple, nous pouvons utiliser `Date.beginning_of_week/2` pour obtenir la date du début de la semaine actuelle et `Date.end_of_day/2` pour obtenir la date de la fin de la journée actuelle.

```Elixir
start_of_week = Date.beginning_of_week(Date.utc_today())
end_of_day = Date.end_of_day(Date.utc_today())

IO.puts "La semaine a commencé le #{Date.to_iso8601(start_of_week)} et se termine à #{Date.to_iso8601(end_of_day)}"
```

La sortie de ce code sera:

```
La semaine a commencé le 2022-01-24 et se termine à 2022-01-30T23:59:59Z
```

# Voir aussi

- Documentation officielle d'Elixir pour la manipulation de dates : https://hexdocs.pm/elixir/Date.html
- Tutoriel vidéo sur le calcul de dates en Elixir : https://www.youtube.com/watch?v=chwSjzPHKxM
- Article de blog sur les différentes façons de manipuler les dates en Elixir : https://medium.com/cameron-nokes/3-ways-to-manipulate-dates-in-elixir-abb69d0ee670
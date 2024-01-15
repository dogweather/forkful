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

# Pourquoi

Les dates sont un élément essentiel de tout système de programmation. La capacité de calculer une date dans le futur ou dans le passé est importante pour de nombreuses applications, telles que les rappels, les planifications et les prévisions. En utilisant Elixir, nous pouvons facilement réaliser cette tâche et gagner du temps dans nos projets.

## Comment faire

Tout d'abord, nous avons besoin d'importer le module `Calendar` en utilisant `require Calendar` ou `import Calendar` dans notre fichier Elixir. Ensuite, nous pouvons utiliser les fonctions du module pour calculer les dates.

Voici un exemple de code pour calculer la date dans 3 jours à partir de la date actuelle :

```elixir
require Calendar

today = Calendar.local_days()
future_date = Calendar.plus(today, 3)

IO.puts("La date dans 3 jours sera : #{future_date.to_string()}")
```

La sortie de ce code sera : "La date dans 3 jours sera : 2021-09-04".

Il est également possible de spécifier une date de départ différente en utilisant la fonction `Date.new/3` qui prend les paramètres de l'année, du mois et du jour. Par exemple, pour calculer la date dans 2 mois à partir du 25 décembre 2021 :

```elixir
require Calendar

start_date = Date.new(2021, 12, 25)
future_date = Calendar.plus(start_date, Month.add(2))

IO.puts("La date dans 2 mois à partir du 25 décembre est : #{future_date.to_string()}")
```

La sortie sera : "La date dans 2 mois à partir du 25 décembre est : 2022-02-25".

## Plongée en profondeur

En utilisant le module `Calendar`, nous pouvons également calculer des dates dans le passé en utilisant la fonction `Calendar.minus/2`. De plus, il existe une variété de fonctions pour manipuler les dates, telles que `Calendar.diff/2` pour calculer la différence entre deux dates, `Calendar.format!/3` pour formater une date selon un modèle spécifique, et bien d'autres encore.

De plus, Elixir nous permet également de travailler avec des dates et des heures en utilisant le module `DateTime`. Ce module offre des fonctions similaires à celles de `Calendar` mais prend également en compte les heures, les minutes et les secondes.

## Voir aussi

- Le module `Calendar` : https://hexdocs.pm/elixir/Calendar.html
- Le module `DateTime` : https://hexdocs.pm/elixir/DateTime.html
- Le langage de programmation Elixir : https://elixir-lang.org/
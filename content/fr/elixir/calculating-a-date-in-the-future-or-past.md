---
title:                "Elixir: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez sur des projets informatiques, vous savez peut-être déjà à quel point les dates peuvent être importantes. Mais parfois, il est nécessaire de gérer des dates dans le futur ou dans le passé. Dans cet article, nous allons parler de la façon de calculer une date dans le futur ou dans le passé en utilisant Elixir.

## Comment faire

Pour commencer, nous allons utiliser la fonction `Calendar.add/3` pour ajouter ou soustraire une quantité spécifiée de temps à une date donnée. Par exemple, si nous voulons calculer la date dans 7 jours à partir d'aujourd'hui, nous pouvons utiliser `Calendar.add(Date.utc_today(), 7, :days)`. Cela utilisera la date UTC actuelle comme point de départ et ajoutera 7 jours.

```Elixir
date_future = Calendar.add(Date.utc_today(), 7, :days)
IO.puts("Date dans 7 jours: #{date_future}")
```

Cela renverra `Date dans 7 jours: 20XX-XX-XX`, en fonction de la date actuelle. De même, si vous voulez calculer une date dans le passé, il suffit de spécifier un nombre négatif comme deuxième argument dans `Calendar.add/3`.

```Elixir
date_passé = Calendar.add(Date.utc_today(), -3, :months)
IO.puts("Date il y a 3 mois: #{date_passé}")
```

Cela renverra `Date il y a 3 mois: 20XX-XX-XX`, encore une fois en fonction de la date actuelle. Vous pouvez également utiliser d'autres unités de temps telles que `:weeks`, `:hours` ou `:minutes` en fonction de vos besoins.

## En profondeur

Si vous voulez plonger un peu plus profondément, Elixir dispose d'une fonctionnalité utile appelée les "timex intervals". Timex est une bibliothèque de gestion des dates et des heures qui étend les fonctionnalités de base d'Elixir. En utilisant Timex, vous pouvez également facilement calculer des dates dans le futur ou dans le passé en fonction des intervalles de temps.

Par exemple, si vous voulez calculer la date qui correspond à un an à partir de maintenant, vous pouvez utiliser `Timex.add(Duration.from_years(1))` pour créer un intervalle d'un an et l'ajouter à la date actuelle. Timex offre également des fonctionnalités avancées pour gérer les fuseaux horaires, les horaires d'été et bien plus encore.

## Voir aussi

- [Documentation sur les fonctions de date d'Elixir](https://hexdocs.pm/elixir/Calendar.html#add/3)
- [Documentation sur Timex](https://hexdocs.pm/timex/Timex.html)
- [Tutoriel sur le calcul des dates avec Elixir](https://blog.appsignal.com/2016/12/20/working-with-dates-and-times-in-elixir.html)
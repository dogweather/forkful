---
title:                "Comparer deux dates"
html_title:           "Elixir: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous travaillez avec des dates dans vos projets Elixir, il est probable que vous ayez à comparer deux dates à un moment donné. Comprendre comment le faire correctement peut vous faire gagner du temps et éviter de potentielles erreurs dans votre code. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Elixir.

## Comment faire 

Pour comparer deux dates en Elixir, nous allons utiliser la fonction `DateTime.diff/2`, qui calcule la différence en secondes entre deux dates. Pour commencer, nous allons créer deux variables contenant des dates différentes :

```Elixir
date1 = DateTime.new(2020, 5, 12)
date2 = DateTime.new(2020, 5, 15)
```

Ensuite, nous appelons la fonction `DateTime.diff/2` en passant nos deux variables en tant que paramètres :

```Elixir
DateTime.diff(date1, date2)
```

Nous obtenons alors en sortie la différence en secondes entre les deux dates, dans cet exemple, il s'agit de 259200 (3 jours).

Pour comparer des dates selon une unité de temps particulière, par exemple les jours, nous pouvons utiliser la fonction `DateTime.diff/3` en lui passant en troisième paramètre `:days`. Dans cet exemple, nous obtenons la différence en jours entre les deux dates :

```Elixir
DateTime.diff(date1, date2, :days)
```

L'avantage de cette approche est qu'elle nous permet de comparer des dates à différentes unités de temps (jours, minutes, heures, etc.) selon nos besoins.

## Plongée en profondeur 

La fonction `DateTime.diff/2` utilise la notation de temps UTC (Temps Universel Coordonné) pour calculer la différence entre deux dates. Cela signifie qu'elle ne prend pas en compte les différences de fuseau horaire. Si vous avez besoin de prendre en compte les fuseaux horaires, il est recommandé d'utiliser la bibliothèque `tzdata` qui fournit des fonctions pour manipuler les dates et les heures locales.

Il est également important de garder à l'esprit que la fonction `DateTime.diff/2` prend en compte les dates et les heures, pas les dates seules. Si vous n'avez besoin de comparer que des dates sans tenir compte des heures, vous pouvez utiliser la fonction `Date.diff/2`.

## Voir aussi 

- Documentation officielle Elixir sur la fonction `DateTime.diff/2`
- Documentation officielle Elixir sur la bibliothèque `tzdata`
- Article "Les dates en Elixir" sur le blog de devonestesur.com (en français)
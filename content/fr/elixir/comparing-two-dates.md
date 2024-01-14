---
title:    "Elixir: Comparer deux dates"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de dates dans la programmation est très courante, notamment lorsqu'il s'agit de suivre les événements dans un système ou de créer des fonctionnalités basées sur des dates spécifiques. Par conséquent, il est souvent nécessaire de comparer deux dates pour déterminer l'ordre chronologique ou pour vérifier si une date est plus récente qu'une autre. Dans cet article, nous allons explorer comment comparer efficacement deux dates en utilisant le langage de programmation Elixir.

## Comment faire

Pour comparer facilement deux dates en Elixir, nous allons utiliser la fonction `Date.compare/2` de la librairie standard d'Elixir. Cette fonction prend deux dates en entrée et renvoie l'une des trois valeurs suivantes :

- `:eq` (equal) si les deux dates sont égales
- `:lt` (less than) si la première date est antérieure à la seconde
- `:gt` (greater than) si la première date est postérieure à la seconde

Regardons un exemple concret :

```Elixir
# Définition des deux dates à comparer
date1 = ~D[2020-01-01]
date2 = ~D[2020-05-15]

# Comparaison des deux dates
Date.compare(date1, date2)
```

La sortie de ce code sera `:lt`, car la date `date1` est antérieure à `date2`.

Nous pouvons également utiliser la fonction `Date.compare/2` avec des heures ou des dates et heures en ajoutant simplement un autre argument facultatif représentant l'heure, comme ceci :

```Elixir
# Définition des deux dates et heures à comparer
datetime1 = ~U[2020-01-01 15:30:00]
datetime2 = ~U[2020-01-01 20:45:00]

# Comparaison des deux date-heures
Date.compare(datetime1, datetime2, :time)
```

La sortie de ce code sera également `:lt`, car `datetime1` se produit avant `datetime2`.

## Plongée en profondeur

Il est important de noter que la fonction `Date.compare/2` compare les dates de manière précise en prenant en compte le fuseau horaire. De plus, si un seul argument est une date et l'autre une date et heure, le comparateur enlève simplement la partie temps de la date et l'utilise pour la comparaison. Cela peut entraîner des résultats inattendus si vous ne faites pas attention aux types de données que vous utilisez.

De plus, la comparaison des dates en utilisant `Date.compare/2` se fait en utilisant l'ordre chronologique naturel des dates. Cela signifie que les années sont comparées en premier, puis les mois et enfin les jours. Si deux dates ont le même jour mais des mois ou des années différents, la comparaison se fera en fonction des mois ou années.

## Voir aussi

- La documentation sur `Date.compare/2` : https://hexdocs.pm/elixir/Date.html#compare/2
- Comment formater les dates en Elixir : https://hexdocs.pm/elixir/Date.html#module-formatting-and-parsing
- Comparaison des dates en utilisant la fonction `Date.before?/2` : https://hexdocs.pm/elixir/Date.html#before?/2
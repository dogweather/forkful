---
title:                "Gleam: Comparer deux dates"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi?

Comparer deux dates peut sembler une tâche simple au premier abord, mais cela peut être très utile lors de la création d'applications qui gèrent des événements ou des tâches planifiées. En utilisant des fonctions de comparaison de dates, vous pouvez facilement déterminer si une date est antérieure, ultérieure ou égale à une autre date. Cela peut être particulièrement pratique lors de la mise en œuvre de validations de formulaire ou de fonctionnalités de rappel pour les tâches à effectuer dans le futur.

## Comment faire?

Pour comparer deux dates en Gleam, vous pouvez utiliser la fonction `DateTime.compare` suivie des deux dates que vous souhaitez comparer. Par exemple, si vous avez deux dates en tant que variables `date1` et `date2`, vous pouvez utiliser la syntaxe suivante pour les comparer :

```Gleam
DateTime.compare(date1, date2)
```

Cette fonction renverra une valeur indiquant si la première date est antérieure, ultérieure ou égale à la seconde date. Voici un exemple de sortie en utilisant un enregistrement de date :

```Gleam
let date1 = Date(2020, 12, 15)
let date2 = Date(2020, 12, 10)
DateTime.compare(date1, date2) // Retourne `after`
DateTime.compare(date2, date1) // Retourne `before`
DateTime.compare(date1, date1) // Retourne `equal`
```

Vous pouvez également utiliser cette fonction pour comparer des dates et des heures. Par exemple, si vous avez une date et une heure en tant que variables `datetime1` et `datetime2`, vous pouvez utiliser la syntaxe suivante pour les comparer :

```Gleam
DateTime.compare(datetime1, datetime2)
```

Cela renverra également une valeur indiquant si la première date et heure sont avant, après ou égales à la seconde. Vous pouvez également utiliser la fonction `DateTime.compare_duration` pour comparer des durées.

## Plongée plus profonde

Le module gleam/datetime contient d'autres fonctions de comparaison de dates qui peuvent être utiles dans des cas spécifiques. Par exemple, `DateTime.is_same_year` vous permet de vérifier si deux dates appartiennent à la même année. `DateTime.is_before` et `DateTime.is_after` peuvent être utilisés pour vérifier si une date est avant ou après une autre, respectivement.

Il est également important de noter que Gleam traite les dates et les heures dans un fuseau horaire spécifique, celui de l'UTC. Lors de la comparaison de dates, assurez-vous de prendre en compte les différences de fuseau horaire si cela est nécessaire pour votre application.

## Voir aussi

- Documentation officielle Gleam pour la comparaison de dates : https://gleam.run/libraries/datetime.html#compare
- Tutoriel sur les dates et heures en Gleam : https://medium.com/@gleamlang/dates-and-times-in-gleam-506e817c8a9e
- Tutoriel sur la manipulation du fuseau horaire en Gleam : https://blog.moyogo.co/managing-timezones-gleam.html
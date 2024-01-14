---
title:    "Gleam: Comparer deux dates"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de comparer deux dates pour déterminer l'ordre chronologique des événements. Cela peut être utile pour trier des données, planifier des tâches, ou vérifier les délais. Dans cet article, nous allons explorer comment réaliser une comparaison de dates en utilisant le langage de programmation Gleam.

## Comment faire

La comparaison de dates peut sembler complexe, mais grâce à Gleam, la syntaxe est simple et intuitive. Tout d'abord, nous devons importer le module `Datetime` pour avoir accès aux fonctions de manipulation de dates.

```
Gleam import Datetime
``` 

Ensuite, nous pouvons créer deux variables de type `Datetime.date` pour représenter nos deux dates à comparer.

```
Gleam let first_date = Datetime.date(2021,7,1)
Gleam let second_date = Datetime.date(2021,8,1)
```

Maintenant que nous avons nos deux dates, nous pouvons utiliser l'opérateur de comparaison `>` pour vérifier si la première date est antérieure à la deuxième.

```
Gleam if first_date > second_date {
  // la première date est après la deuxième
}
```

Nous pouvons également utiliser les opérateurs `>=` et `==` pour vérifier si les dates sont postérieures ou égales, ainsi que `<=` pour vérifier si elles sont antérieures ou égales.

## Plongée en profondeur

En plus de l'opérateur de comparaison, le module `Datetime` offre des fonctions utiles pour manipuler les dates. Par exemple, nous pouvons utiliser la fonction `add_days` pour ajouter un nombre spécifique de jours à une date.

```
Gleam let new_date = Datetime.add_days(first_date, 7)
```

Il est également possible d'utiliser des valeurs négatives pour soustraire des jours à la date. De plus, le module `Datetime` prend en compte les années bissextiles et les mois avec un nombre variable de jours, assurant une précision dans les calculs de dates.

## Voir aussi

Pour plus d'informations sur la comparaison de dates en Gleam, vous pouvez consulter les ressources suivantes:

- [Documentation officielle de Gleam sur les dates](https://gleam.run/documentation/guide/working-with-datetime.html)
- [Tutoriel sur les dates en Gleam](https://gleam.run/documentation/tutorials/dates.html)
- [Exemples de code sur la comparaison de dates en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/dates.gleam)
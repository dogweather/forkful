---
title:    "Gleam: Calculer une date dans le futur ou le passé"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé peut être utile pour de nombreux cas d'utilisation, comme planifier des rendez-vous, des événements ou des rappels, ou encore pour suivre l'historique des tâches et des activités.

## Comment faire

Pour calculer une date future ou passée en utilisant Gleam, il existe quelques étapes simples à suivre :

1. Importez le module de dates en utilisant `import date`.
2. Utilisez `date.add()` pour ajouter ou soustraire un nombre donné de jours, semaines, mois ou années à une date donnée.
3. Utilisez `date.format()` pour afficher la date dans le format souhaité.

Voici un exemple de code pour calculer la date dans le futur et afficher le résultat dans le format "jour/mois/année" :

```
Gleam
import date

let date = date.Date(year=2020, month=12, day=2)
let future_date = date.add({ days = 10 })

let formatted_date = date.format(
  date = future_date,
  format = { day = "d", month = "m", year = "y" }
)

debug.formatted("La date dans 10 jours sera {}", [formatted_date])
```

Lorsque vous exécutez ce code, vous obtiendrez une sortie similaire à celle-ci :

```
La date dans 10 jours sera 12/12/2020
```

## Plongée en profondeur

En utilisant ce même exemple, vous pouvez également spécifier des valeurs négatives pour soustraire des jours, semaines, mois ou années à une date donnée. De plus, vous pouvez également utiliser `date.add_business_days()` pour exclure les weekends lors du calcul d'une date future ou passée. Enfin, en utilisant `date.to_int()` vous pouvez convertir une date en entier afin de la manipuler facilement dans des opérations arithmétiques.

## Voir aussi

- [Documentation sur les dates et heures en Gleam](https://gleam.run/documentation/standard_library/date_time/)
- [Tutoriel sur les dates et heures en Gleam](https://www.codementor.io/@ingwene/today-s-date-and-time-in-gleam-p06rraugz) (en anglais)
- [Exemples de code pour calculer des dates en Gleam](https://www.programiz.com/gleam-programming/examples/get-date-time) (en anglais)
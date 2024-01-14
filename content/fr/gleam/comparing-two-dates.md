---
title:    "Gleam: Comparaison de deux dates"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

La fonction de comparaison de dates est un élément essentiel de tout programme de gestion du temps. Elle permet de vérifier si une date est antérieure, égale ou postérieure à une autre, ce qui peut être utile pour la planification et l'organisation des tâches.

## Comment faire

Voici un exemple de code en Gleam pour comparer deux dates :

```Gleam
import gleam/date

let aujourdhui = date.now()
let demain = date.add_days(aujourdhui, 1)

if date.compare(aujourdhui, demain) == date.Equal {
  io.println("Ces deux dates sont égales.")
} else if date.compare(aujourdhui, demain) == date.Less {
  io.println("La deuxième date vient après la première.")
} else {
  io.println("La première date vient après la deuxième.")
}
```

Output :

```
La deuxième date vient après la première.
```

## Plongée en profondeur

La fonction de comparaison de dates en Gleam utilise une énumération pour définir les résultats possibles : Equal (égales), Greater (supérieure) et Less (inférieure). Elle se base sur la norme ISO 8601 pour comparer les dates et prend en compte les fuseaux horaires et les années bissextiles.

Il est également possible de comparer les heures et les minutes en utilisant la fonction `compare_time()`.

## Voir aussi

- Documentation officielle de Gleam sur les dates : https://gleam.run/libraries/dates
- Norme ISO 8601 : https://www.iso.org/fr/standard/70907.html
- Exemple de comparaison de dates en Java : https://www.baeldung.com/java-compare-dates
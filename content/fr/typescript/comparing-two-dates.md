---
title:                "TypeScript: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi comparer deux dates en TypeScript?

Comparer des dates peut sembler être une tâche simple, mais en réalité cela peut être assez complexe, surtout en programmation. En TypeScript, il est important de comprendre comment comparer deux dates, car cela peut être utile dans de nombreuses situations, comme la gestion de tâches planifiées, la validation de données ou même dans la création de fonctionnalités de calendrier.

## Comment comparer deux dates en TypeScript?

Pour comparer deux dates en TypeScript, il existe différentes méthodes. Voici un exemple de code utilisant la méthode `getTime()` pour comparer deux dates :

```
let date1 = new Date('March 15, 2021');
let date2 = new Date('March 20, 2021');

// Utilisation de la méthode getTime() pour comparer les dates
if (date1.getTime() < date2.getTime()) {
  console.log('La date 1 est antérieure à la date 2.');
} else if (date1.getTime() > date2.getTime()) {
  console.log('La date 1 est postérieure à la date 2.');
} else {
  console.log('Les deux dates sont identiques.');
}
```

La sortie de ce code sera : `La date 1 est antérieure à la date 2.` Cela est dû au fait que la méthode `getTime()` renvoie le nombre de millisecondes écoulées depuis le 1er janvier 1970 pour chaque date. Ainsi, la date 1 est bien antérieure à la date 2.

Il est également possible de comparer les dates en utilisant les opérateurs de comparaison (`<`, `>`, `===`), mais il faut faire attention car ils comparent également les heures, minutes et secondes.

## Plongée en profondeur 

Lors de la comparaison de dates en TypeScript, il est important de comprendre les différentes options de comparaison et de trouver celle qui correspond le mieux à vos besoins. Par exemple, si vous voulez comparer uniquement les dates sans prendre en compte les heures, minutes et secondes, vous pouvez utiliser la méthode `toISOString()` pour convertir les dates en chaînes de caractères et les comparer ensuite.

Il est également important de prendre en compte la gestion du temps universel (UTC), car cela peut affecter le résultat de la comparaison selon les fuseaux horaires.

## Voir aussi

- [Documentation officielle de TypeScript sur la classe Date](https://www.typescriptlang.org/docs/handbook/classes.html#date)
- [Article du site MDN sur la comparaison de dates en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Exemple de code pour comparer les dates en TypeScript](https://www.tutorialspoint.com/typescript/date_compare.htm)